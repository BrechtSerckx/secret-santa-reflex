module SecretSanta.UI where


import qualified Data.Text.Lazy                as TL
import qualified Text.Pretty.Simple            as Pretty

import qualified Reflex                        as Rx
import qualified Reflex.Dom                    as Rx
import           Reflex.Dom                     ( (=:) )
import qualified Servant.Reflex                as SR

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
  styleSheet link =
    Rx.elAttr "link"
              [("rel", "stylesheet"), ("type", "text/css"), ("href", link)]
      $ pure ()

bodyWidget :: forall t m . Rx.MonadWidget t m => m ()
bodyWidget =
  let cCreateSecretSanta = SR.client
        api
        (Proxy @m)
        (Proxy @())
        (Rx.constDyn $ SR.BaseFullUrl SR.Http "localhost" 8080 "/")
  in  Rx.elClass "section" "section" . Rx.elClass "div" "container" $ do
        Rx.elClass "h1" "title is-1" $ Rx.text "Secret Santa"
        Rx.el "p" $ Rx.text "My first website with Bulma!"

        Rx.el "hr" $ pure ()

        -- form
        rec eFormSubmitted <-
              Rx.switch
              .   Rx.current
              <$> Rx.widgetHold (Rx.elClass "div" "block" formWidget) eHideForm
            let eHideForm = (Rx.blank $> Rx.never) <$ eSuccess
            dReqBody <- Rx.holdDyn (Left "") $ Right <$> eFormSubmitted
            eReqRes  <- cCreateSecretSanta dReqBody (void eFormSubmitted)
            let eSuccess = Rx.fmapMaybe SR.reqSuccess eReqRes
        Rx.widgetHold_ Rx.blank $ eReqRes <&> \case
          SR.ResponseSuccess _ () _ -> Rx.blank
          SR.ResponseFailure _ t  _ -> displayErr t
          SR.RequestFailure _ t     -> displayErr t
        Rx.widgetHold_ Rx.blank
          $   Rx.elClass "div" "notification is-success"
          .   Rx.text
          .   TL.toStrict
          .   Pretty.pShowNoColor
          <$> Rx.tagPromptlyDyn dReqBody eSuccess
  where displayErr = Rx.elClass "el" "notification is-danger" . Rx.text

