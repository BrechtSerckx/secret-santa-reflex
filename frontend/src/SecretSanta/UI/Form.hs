module SecretSanta.UI.Form
  ( formWidget
  , formDisplayWidget
  , participantsWidget
  ) where

import           Control.Lens
import           Control.Monad.Fix
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Time                     as Time
import qualified Text.Pretty.Simple            as Pretty

import qualified Reflex                        as Rx
import qualified Reflex.Dom                    as Rx
import           Reflex.Dom                     ( (=:) )
import qualified Servant.Reflex                as SR

import           SecretSanta.Data

data Submit = Submit

formWidget
  :: (Rx.DomBuilder t m, MonadFix m, Rx.MonadHold t m, Rx.PostBuild t m)
  => m (Rx.Event t Form)
formWidget = do
  Rx.el "form" $ do
    wDescription    <- descriptionWidget
    wDate           <- dateWidget
    wPrice          <- priceWidget
    eNewParticipant <- newParticipantWidget
    wParticipants   <- participantsWidget eNewParticipant
    eSubmit         <- submitWidget
    let submit = do
          fDescription  <- wDescription
          fDate         <- wDate
          fPrice        <- wPrice
          fParticipants <- pure []
          pure Form { .. }
    pure . Rx.tag submit $ eSubmit

descriptionWidget :: Rx.DomBuilder t m => m (Rx.Behavior t Text)
descriptionWidget = Rx.elClass "div" "field" $ do
  Rx.elClass "label" "label" $ Rx.text "Description"
  let inputAttrs = mconcat
        [ "placeholder" =: "Description of the event"
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

dateWidget :: Rx.DomBuilder t m => m (Rx.Behavior t Text)
dateWidget = Rx.elClass "div" "field" $ do
  Rx.elClass "label" "label" $ Rx.text "Date"
  let inputAttrs = mconcat ["class" =: "input", "type" =: "date"]
  input <-
    Rx.elClass "div" "control"
    . Rx.inputElement
    $ def
    & (  Rx.inputElementConfig_elementConfig
      .  Rx.elementConfig_initialAttributes
      .~ inputAttrs
      )
  -- TODO
  let mkDate = identity
  pure . fmap mkDate . Rx.current . Rx._inputElement_value $ input

priceWidget :: Rx.DomBuilder t m => m (Rx.Behavior t Double)
priceWidget = Rx.elClass "div" "field" $ do
  Rx.elClass "label" "label" $ Rx.text "Price"
  let inputAttrs = mconcat
        [ "placeholder" =: "Price of the event"
        , "class" =: "input"
        , "type" =: "number"
        ]
  input <-
    Rx.elClass "div" "control"
    . Rx.inputElement
    $ def
    & (  Rx.inputElementConfig_elementConfig
      .  Rx.elementConfig_initialAttributes
      .~ inputAttrs
      )
  -- TODO
  let mkPrice = fromMaybe 0 . readMaybe . T.unpack
  pure . fmap mkPrice . Rx.current . Rx._inputElement_value $ input

newParticipantWidget
  :: (Rx.DomBuilder t m, MonadFix m) => m (Rx.Event t Participant)
newParticipantWidget = Rx.el "div" $ do
  Rx.elClass "label" "label" $ Rx.text "Name"
  rec dName              <- nameWidget eSubmitParticipant
      eSubmitParticipant <- submitParticipantWidget
  pure $ Rx.tag dName eSubmitParticipant
 where
  nameWidget e = do
    input <-
      Rx.elClass "div" "control"
      . Rx.inputElement
      $ def
      & (Rx.inputElementConfig_setValue .~ fmap (const "") e)
      & (  Rx.inputElementConfig_elementConfig
        .  Rx.elementConfig_initialAttributes
        .~ mconcat
             [ "placeholder" =: "Participant name"
             , "class" =: "input"
             , "type" =: "text"
             ]
        )
    pure . Rx.current . Rx._inputElement_value $ input
  submitParticipantWidget =
    Rx.elClass "div" "field" . Rx.elClass "div" "control" $ do
      let btnAttrs = [("class", "button is-link"), ("type", "button")]
      (e, _) <-
        Rx.element "button"
                   (def & Rx.elementConfig_initialAttributes .~ btnAttrs)
          $ Rx.text "Add Participant"
      pure $ Rx.domEvent Rx.Click e

participantsWidget
  :: forall t m
   . (Rx.DomBuilder t m, MonadFix m, Rx.MonadHold t m, Rx.PostBuild t m)
  => Rx.Event t Participant
  -> m ()
participantsWidget eNewParticipant = Rx.el "div" $ do
  rec dCurrParticipants <- Rx.foldDyn ($) Map.empty
        $ Rx.mergeWith (.) [eNewParticipant', eDeleteParticipants dPDeleteMap]
      dPDeleteMap <- listParticipantsWidget dCurrParticipants
  pure ()

 where
  eNewParticipant' :: Rx.Event t (Map Int Participant -> Map Int Participant)
  eNewParticipant' = new <$> eNewParticipant
  eDeleteParticipants m =
    Rx.switch
      . Rx.current
      . Rx.ffor m
      $ Rx.mergeWith (.)
      . map (fmap Map.delete)
      . Map.elems
  listParticipantsWidget
    :: Rx.Dynamic t (Map Int Text)
    -> m (Rx.Dynamic t (Map Int (Rx.Event t Int)))
  listParticipantsWidget ps =
    Rx.elClass "ul" "list" $ Rx.listWithKey ps $ \k p ->
      Rx.elClass "li" "element" $ do
        Rx.dynText $ fmap (("Key: " <> show k <> " for name ") <>) p
        let btnAttrs = [("class", "button is-link"), ("type", "button")]
        (e, _) <-
          Rx.element "button"
                     (def & Rx.elementConfig_initialAttributes .~ btnAttrs)
            $ Rx.text "x"
        pure $ const k <$> Rx.domEvent Rx.Click e
  new :: a -> Map Int a -> Map Int a
  new v m = case Map.maxViewWithKey m of
    Nothing          -> [(0, v)]
    Just ((k, _), _) -> Map.insert (succ k) v m


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
      mkForm f = Rx.text . TL.toStrict . Pretty.pShowNoColor $ f
      displayReqRes rr = case rr of
        SR.ResponseSuccess _ f _ -> mkForm f
        SR.ResponseFailure _ t _ -> displayErr t
        SR.RequestFailure _ t    -> displayErr t
  Rx.widgetHold_ Rx.blank $ displayReqRes <$> eReqRes
