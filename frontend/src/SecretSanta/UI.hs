module SecretSanta.UI where


import qualified Reflex                        as Rx
import qualified Reflex.Dom                    as Rx
import           Reflex.Dom                     ( (=:) )
import qualified Servant.Reflex                as SR

import           SecretSanta.API
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
  let echoForm = SR.client
        (Proxy @API)
        (Proxy @m)
        (Proxy @())
        (Rx.constDyn $ SR.BaseFullUrl SR.Http "localhost" 8080 "/")
  in  Rx.elClass "section" "section" . Rx.elClass "div" "container" $ do
                                                -- header
        Rx.elClass "h1" "title" $ Rx.text "Secret Santa"
        Rx.el "p" $ Rx.text "My first website with Bulma!"

        Rx.el "hr" $ pure ()

        -- form
        eFormSubmitted <- formWidget
        formDisplayWidget eFormSubmitted echoForm

