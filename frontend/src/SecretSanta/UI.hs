{-# LANGUAGE NoMonomorphismRestriction #-}
module SecretSanta.UI where


import qualified Reflex                        as Rx
import qualified Reflex.Dom                    as Rx
import qualified Servant.Reflex                as SR

import           Config                         ( baseUrl )
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
      -- first show form, wheb event fires show nothing
      eFormSubmitted <- Rx.switch . Rx.current <$> Rx.widgetHold
        formWidget'
        (eSuccess $> dummyFormWidget)
      dReqBody <- Rx.traceDyn "reqbody" <$> mkReqBody eFormSubmitted
      eReqRes <- cCreateSecretSanta dReqBody $ Rx.traceEvent "submit req" $ void
        eFormSubmitted
      let eSuccess = Rx.fmapMaybe SR.reqSuccess eReqRes

  -- show errors
  Rx.widgetHold_ Rx.blank $ mkErrorWidget <$> eReqRes
  -- show response if success
  Rx.widgetHold_ Rx.blank $ mkSuccessWidget <$> eSuccess


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
    const
      $ Rx.elClass "div" "notification is-success"
      . Rx.text
      $ "Secret Santa successfully submitted!"


cCreateSecretSanta
  :: forall t m
   . Rx.MonadWidget t m
  => Rx.Dynamic t (Either Text Form)
  -> Rx.Event t ()
  -> m (Rx.Event t (SR.ReqResult () ()))

--brittany-disable-next-binding
( cCreateSecretSanta
  ) = client

client :: forall t m . Rx.MonadWidget t m => SR.Client t m API ()
client = SR.client api (Proxy @m) (Proxy @()) $ Rx.constDyn baseUrl
