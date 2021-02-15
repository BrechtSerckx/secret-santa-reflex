module Main where

import           Control.Lens
import           Control.Monad.Fix              ( MonadFix )
import           Data.Default.Class             ( Default(..) )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( ($>) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Reflex                        as Rx
import qualified Reflex.Dom                    as Rx
import           Reflex.Dom                     ( (=:) )
import           Text.Read                      ( readMaybe )


data Form = Form
  { fName     :: Text
  , fLocation :: Text
  }
  deriving Show
data Submit = Submit


main :: IO ()
main = Rx.mainWidgetWithHead headWidget bodyWidget

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

bodyWidget :: Rx.MonadWidget t m => m ()
bodyWidget = Rx.elClass "section" "section" . Rx.elClass "div" "container" $ do
  -- header
  Rx.elClass "h1" "title" $ Rx.text "Secret Santa"
  Rx.el "p" $ Rx.text "My first website with Bulma!"

  Rx.el "hr" $ pure ()

  -- form
  eFormSubmitted <- formWidget
  formDisplayWidget eFormSubmitted

  Rx.el "hr" $ pure ()

  eIpAddr <- mkEIpAddr eFormSubmitted
  ipAddrWidget eIpAddr

formWidget :: Rx.DomBuilder t m => m (Rx.Event t Form)
formWidget = Rx.el "form" $ do
  wName     <- nameWidget
  wLocation <- locationWidget
  eSubmit   <- submitWidget
  let submit = do
        fName     <- wName
        fLocation <- wLocation
        pure Form { .. }
  pure . Rx.tag submit $ eSubmit

nameWidget :: Rx.DomBuilder t m => m (Rx.Behavior t Text)
nameWidget = Rx.elClass "div" "field" $ do
  Rx.elClass "label" "label" $ Rx.text "Name"
  let inputAttrs = mconcat
        [ "placeholder" =: "Name of the event"
        , "class" =: "input"
        , "type" =: "text"
        ]
  input <-
    Rx.elClass "div" "control"
    . Rx.inputElement
    $ def
    & (  Rx.inputElementConfig_elementConfig
      .  Rx.elementConfig_initialAttributes
      .~ inputAttrs
      )
  pure . Rx.current . Rx._inputElement_value $ input

locationWidget :: Rx.DomBuilder t m => m (Rx.Behavior t Text)
locationWidget = Rx.elClass "div" "field" $ do
  Rx.elClass "label" "label" $ Rx.text "Location"
  let inputAttrs = mconcat
        [ "placeholder" =: "Location of the event"
        , "class" =: "input"
        , "type" =: "text"
        ]
  input <-
    Rx.elClass "div" "control"
    . Rx.inputElement
    $ def
    & (  Rx.inputElementConfig_elementConfig
      .  Rx.elementConfig_initialAttributes
      .~ inputAttrs
      )
  pure . Rx.current . Rx._inputElement_value $ input

submitWidget :: Rx.DomBuilder t m => m (Rx.Event t Submit)
submitWidget = Rx.elClass "div" "field" . Rx.elClass "div" "control" $ do
  let btnAttrs = [("class", "button is-link"), ("type", "button")]
  (e, _) <-
    Rx.element "button" (def & Rx.elementConfig_initialAttributes .~ btnAttrs)
      $ Rx.text "Submit"
  pure $ Rx.domEvent Rx.Click e $> Submit

formDisplayWidget
  :: (Rx.DomBuilder t m, Rx.MonadHold t m, Rx.PostBuild t m)
  => Rx.Event t Form
  -> m ()
formDisplayWidget eFormSubmitted =
  let mkForm Form {..} = Rx.el "ul" $ do
        Rx.el "li" . Rx.text $ "Name: " <> fName
        Rx.el "li" . Rx.text $ "Location: " <> fLocation
  in  Rx.widgetHold_ Rx.blank $ mkForm <$> eFormSubmitted

mkEIpAddr :: Rx.MonadWidget t m => Rx.Event t a -> m (Rx.Event t Text)
mkEIpAddr e =
  let getIp r = case r ^. Rx.xhrResponse_responseText of
        Just t  -> t
        Nothing -> error "unknown response body"
  in  fmap (fmap getIp)
        .  Rx.performRequestAsync
        $  e
        $> Rx.XhrRequest "GET" "http://httpbin.org/ip" def

ipAddrWidget
  :: (Rx.DomBuilder t m, Rx.MonadHold t m, Rx.PostBuild t m)
  => Rx.Event t Text
  -> m ()
ipAddrWidget eIpAddr = Rx.widgetHold_ Rx.blank $ Rx.text <$> eIpAddr
