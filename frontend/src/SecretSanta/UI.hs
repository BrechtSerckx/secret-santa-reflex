{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
module SecretSanta.UI where

import qualified "common" Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy          as BSL

import qualified Reflex                        as Rx
import qualified Reflex.Dom                    as Rx
import qualified Servant.API                   as S
import           Servant.API                    ( (:<|>)(..) )
import qualified Servant.API.UVerb             as S
import qualified Servant.Reflex                as SR

import           Config                         ( baseUrl )
import           Network.Http.Error

import           SecretSanta.API
import           SecretSanta.Data
import           SecretSanta.UI.Form

ui :: IO ()
ui = Rx.mainWidgetWithHead headWidget bodyWidget

headWidget :: Rx.MonadWidget t m => m ()
headWidget = do
  Rx.el "title" $ Rx.text "Secret Santa"
  Rx.elAttr
    "meta"
    [("name", "viewport"), ("content", "width=device-width, initial-scale=1")]
    Rx.blank
  styleSheet "https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css"
 where
  styleSheet href =
    Rx.elAttr "link"
              [("rel", "stylesheet"), ("type", "text/css"), ("href", href)]
      $ pure ()

bodyWidget :: forall t m . Rx.MonadWidget t m => m ()
bodyWidget = Rx.elClass "section" "section" . Rx.elClass "div" "container" $ do
  Rx.elClass "h1" "title is-1" $ Rx.text "Secret Santa"
  Rx.elClass "h3" "subtitle is-3" $ Rx.text "Match up with your friends!"

  Rx.el "hr" $ pure ()

  -- form
  rec
      -- first show form, when event fires show nothing
      eFormSubmitted <- Rx.switch . Rx.current <$> Rx.widgetHold
        (Rx.elClass "div" "block" formWidget)
        (eSuccess $> (Rx.blank $> Rx.never))
      dReqBody <- Rx.traceDyn "reqbody" <$> mkReqBody eFormSubmitted
      eReqRes  <-
        fmap (fmap parseReqResult)
        . cCreateSecretSanta dReqBody
        $ Rx.traceEvent "submit req"
        $ void eFormSubmitted
      let eSuccess = Rx.fforMaybe eReqRes $ \case
            Left  _ -> Nothing
            Right r -> Just r

  -- success or errors widget
  Rx.widgetHold_ Rx.blank $ eReqRes <&> \case
    Left err -> displayErr err
    Right u ->
      fromMaybe
          (         throwErrorPure
          $         mkError "Unexpected return type"
          `errWhen` "making secret-santa return widget"
          )
        . foldl' @[] (<|>) Nothing
        $ [ S.matchUnion @(S.WithStatus 200 SecretSantaId) u <&> \id ->
            Rx.elClass "div" "notification is-success"
              .  Rx.text
              $  "Secret Santa successfully submitted! Id: "
              <> show id
          , S.matchUnion @InvalidDateTimeError u
          <&> displayErr
          .   errMessage
          .   unGenericError
          .   unApiError
          ]
 where
  displayErr = Rx.elClass "div" "notification is-danger" . Rx.el "p" . Rx.text
  mkReqBody  = Rx.holdDyn (Left "") . fmap Right

parseReqResult :: SR.ReqResult tag a -> Either Text a
parseReqResult = \case
  SR.ResponseSuccess _ a _ -> Right a
  SR.ResponseFailure _ err b ->
    Left . fromMaybe err $ Rx._xhrResponse_response b <&> \case
      Rx.XhrResponseBody_Default t -> t
      Rx.XhrResponseBody_Text    t -> t
      Rx.XhrResponseBody_Blob    _ -> "<blob>"
      Rx.XhrResponseBody_ArrayBuffer t ->
        case Aeson.decode' @InternalError $ BSL.fromStrict t of
          Just e  -> errMessage . unGenericError $ unApiError e
          Nothing -> "<arraybuffer>"
  SR.RequestFailure _ err -> Left err


cCreateSecretSanta
  :: forall t m
   . Rx.MonadWidget t m
  => Rx.Dynamic t (Either Text SecretSantaCreate)
  -> Rx.Event t ()
  -> m
       ( Rx.Event
           t
           ( SR.ReqResult
               ()
               ( S.Union
                   '[ S.WithStatus 200 SecretSantaId
                    , InvalidDateTimeError
                    ]
               )
           )
       )

--brittany-disable-next-binding
( cCreateSecretSanta
  :<|> _
  ) = client

client :: forall t m . Rx.MonadWidget t m => SR.Client t m API ()
client = SR.client api (Proxy @m) (Proxy @()) $ Rx.constDyn baseUrl

instance SR.AuthClient TokenAuth where
  type AuthClientData TokenAuth = ()
  mkAuthReq = const identity
