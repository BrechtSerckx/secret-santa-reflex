{-# LANGUAGE NoMonomorphismRestriction #-}
module SecretSanta.UI where


import qualified Data.Text.Lazy                as TL
import qualified Text.Pretty.Simple            as Pretty

import qualified Reflex                        as Rx
import qualified Reflex.Dom                    as Rx
import           Reflex.Dom                     ( (=:) )
import           Servant.API                    ( (:<|>)(..) )
import qualified Servant.Reflex                as SR

import           Config                         ( baseUrl )
import           SecretSanta.API
import           SecretSanta.Data
import           SecretSanta.UI.Form

enablePing :: Bool
enablePing = False


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
  styleSheet link =
    Rx.elAttr "link"
              [("rel", "stylesheet"), ("type", "text/css"), ("href", link)]
      $ pure ()

bodyWidget :: forall t m . Rx.MonadWidget t m => m ()
bodyWidget = Rx.elClass "section" "section" . Rx.elClass "div" "container" $ do
  Rx.elClass "h1" "title is-1" $ Rx.text "Secret Santa"
  Rx.elClass "h3" "subtitle is-3" $ Rx.text "Match up with your friends!"

  Rx.el "hr" $ pure ()

  -- form
  rec
      -- first show form, wheb event fires show nothing
      eFormSubmitted <-
        Rx.traceEvent "form submitted"
        .   Rx.switch
        .   Rx.current
        <$> Rx.widgetHold formWidget' (eSuccess $> dummyFormWidget)
      dReqBody <- Rx.traceDyn "reqbody" <$> mkReqBody eFormSubmitted
      eReqRes <- cCreateSecretSanta dReqBody $ Rx.traceEvent "submit req" $ void
        eFormSubmitted
      let eSuccess =
            Rx.traceEvent "success"
              . Rx.fmapMaybe SR.reqSuccess
              . Rx.traceEventWith (const "reqres")
              $ eReqRes

  -- show errors
  Rx.widgetHold_ Rx.blank $ mkErrorWidget <$> eReqRes
  -- show response if success
  Rx.widgetHold_ Rx.blank
    $   mkSuccessWidget
    <$> Rx.tagPromptlyDyn dReqBody eSuccess

  when enablePing $ do
    let dPing = Rx.traceDyn "ping" . Rx.constDyn $ Right ()
    ePong <- cPing dPing $ void eFormSubmitted
    Rx.widgetHold_ (Rx.text "ping") $ const (Rx.text "pong") <$> ePong


 where
  displayErr      = Rx.elClass "el" "notification is-danger" . Rx.text
  formWidget'     = Rx.elClass "div" "block" formWidget
  dummyFormWidget = Rx.blank $> Rx.never
  mkReqBody       = Rx.holdDyn (Left "") . fmap Right
  mkErrorWidget   = \case
    SR.ResponseSuccess _ () _ -> Rx.blank
    SR.ResponseFailure _ t  _ -> displayErr t
    SR.RequestFailure _ t     -> displayErr t
  mkSuccessWidget =
    Rx.elClass "div" "notification is-success"
      . Rx.text
      . TL.toStrict
      . Pretty.pShowNoColor


cPing
  :: forall t m
   . Rx.MonadWidget t m
  => Rx.Dynamic t (Either Text ())
  -> Rx.Event t ()
  -> m (Rx.Event t (SR.ReqResult () ()))
cCreateSecretSanta
  :: forall t m
   . Rx.MonadWidget t m
  => Rx.Dynamic t (Either Text Form)
  -> Rx.Event t ()
  -> m (Rx.Event t (SR.ReqResult () ()))

--brittany-disable-next-binding
(      cPing
  :<|> cCreateSecretSanta
  ) = client

client :: forall t m . Rx.MonadWidget t m => SR.Client t m API ()
client = SR.client api (Proxy @m) (Proxy @()) $ Rx.constDyn baseUrl
