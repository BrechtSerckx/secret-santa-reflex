module SecretSanta.UI.Form
  ( formWidget
  , formDisplayWidget
  ) where

import           Control.Lens

import qualified Reflex                        as Rx
import qualified Reflex.Dom                    as Rx
import           Reflex.Dom                     ( (=:) )
import qualified Servant.Reflex                as SR

import           SecretSanta.Data

data Submit = Submit

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
  :: forall t m
   . (Rx.DomBuilder t m, Rx.MonadHold t m, Rx.PostBuild t m)
  => Rx.Event t Form
  -> (  Rx.Dynamic t (Either Text Form)
     -> Rx.Event t ()
     -> m (Rx.Event t (SR.ReqResult () Form))
     )
  -> m ()
formDisplayWidget eFormSubmitted echoForm = do
  dReqBody <- Rx.holdDyn (Left "") $ Right <$> eFormSubmitted
  eReqRes  <- echoForm dReqBody $ void eFormSubmitted
  let displayErr = Rx.text
      mkForm Form {..} = Rx.el "ul" $ do
        Rx.el "li" . Rx.text $ "Name: " <> fName
        Rx.el "li" . Rx.text $ "Location: " <> fLocation
      displayReqRes rr = case rr of
        SR.ResponseSuccess _ f _ -> mkForm f
        SR.ResponseFailure _ t _ -> displayErr t
        SR.RequestFailure _ t    -> displayErr t
  Rx.widgetHold_ Rx.blank $ displayReqRes <$> eReqRes
