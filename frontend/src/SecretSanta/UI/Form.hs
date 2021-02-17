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
    wDescription <- descriptionWidget
    wDate        <- dateWidget
    wPrice       <- priceWidget
    rec wParticipants              <- participantsWidget eNewParticipant
        (eNewParticipant, eSubmit) <- buttonsWidget
    let submit = do
          fDescription  <- wDescription
          fDate         <- wDate
          fPrice        <- wPrice
          fParticipants <- Map.elems <$> wParticipants
          pure Form { .. }
    pure . Rx.tag submit $ eSubmit

descriptionWidget :: Rx.DomBuilder t m => m (Rx.Behavior t Text)
descriptionWidget = fieldHorizontal $ do
  label "Description"
  let inputAttrs = mconcat
        [ "placeholder" =: "Description of the event"
        , "class" =: "input"
        , "type" =: "text"
        ]
  input <-
    fieldBody
    . field
    . control
    . Rx.inputElement
    $ def
    & (  Rx.inputElementConfig_elementConfig
      .  Rx.elementConfig_initialAttributes
      .~ inputAttrs
      )
  pure . Rx.current . Rx._inputElement_value $ input

dateWidget :: Rx.DomBuilder t m => m (Rx.Behavior t Text)
dateWidget = fieldHorizontal $ do
  label "Date"
  let inputAttrs = mconcat ["class" =: "input", "type" =: "date"]
  input <-
    fieldBody
    . field
    . control
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
priceWidget = fieldHorizontal $ do
  label "Price"
  let inputAttrs = mconcat
        [ "placeholder" =: "Price of the event"
        , "class" =: "input"
        , "type" =: "number"
        ]
  input <-
    fieldBody
    . field
    . control
    . Rx.inputElement
    $ def
    & (  Rx.inputElementConfig_elementConfig
      .  Rx.elementConfig_initialAttributes
      .~ inputAttrs
      )
  -- TODO
  let mkPrice = fromMaybe 0 . readMaybe . T.unpack
  pure . fmap mkPrice . Rx.current . Rx._inputElement_value $ input

data AddParticipant = AddParticipant

newParticipantWidget
  :: (Rx.DomBuilder t m, MonadFix m) => m (Rx.Event t AddParticipant)
newParticipantWidget = fieldHorizontal . control $ do
  let btnAttrs = [("class", "button is-link"), ("type", "button")]
  (e, _) <-
    Rx.element "button" (def & Rx.elementConfig_initialAttributes .~ btnAttrs)
      $ Rx.text "Add Participant"
  pure $ Rx.domEvent Rx.Click e $> AddParticipant

type ParticipantMap = Map Int Participant
initialParticipants :: ParticipantMap
initialParticipants = [(0, emptyParticipant), (1, emptyParticipant)]
emptyParticipant :: Participant
emptyParticipant = Participant "" ""
addNewParticipant m = case Map.maxViewWithKey m of
  Nothing          -> [(0, emptyParticipant)]
  Just ((k, _), _) -> Map.insert (succ k) emptyParticipant m

participantsWidget
  :: (Rx.DomBuilder t m, Rx.MonadHold t m, MonadFix m, Rx.PostBuild t m)
  => Rx.Event t AddParticipant
  -> m (Rx.Behavior t ParticipantMap)
participantsWidget eAddNewParticipant = do
  rec
    dCurrParticipants <-
      Rx.foldDyn ($) initialParticipants
      . Rx.mergeWith (.)
      $ [ addNewParticipant <$ eAddNewParticipant
        , overParticipants (\(DeleteParticipant i) -> Map.delete i)
        $   map fst
        <$> dParticipantsMap
        , overParticipants (\(UpdateParticipant i n) -> Map.adjust (const n) i)
        $   map snd
        <$> dParticipantsMap
        ]
    dParticipantsMap <- Rx.listWithKey dCurrParticipants
      $ \k p -> participantWidget k p
  pure . Rx.current $ dCurrParticipants
 where
  overParticipants f m =
    Rx.switch
      . Rx.current
      . Rx.ffor m
      $ Rx.mergeWith (.)
      . map (fmap f)
      . Map.elems

data DeleteParticipant = DeleteParticipant Int
data UpdateParticipant = UpdateParticipant Int Participant

participantWidget
  :: (Rx.DomBuilder t m, Rx.PostBuild t m)
  => Int
  -> Rx.Dynamic t Participant
  -> m ((Rx.Event t DeleteParticipant, Rx.Event t UpdateParticipant))
participantWidget k p = fieldHorizontal $ do
  label "Participant"
  fieldBody $ do
    wName <-
      field
      . control
      . Rx.inputElement
      $ def
      & (  Rx.inputElementConfig_elementConfig
        .  Rx.elementConfig_initialAttributes
        .~ mconcat
             [ "placeholder" =: "Participant name"
             , "class" =: "input"
             , "type" =: "text"
             ]
        )
    wEmail <-
      field
      . control' "expanded"
      . Rx.inputElement
      $ def
      & (  Rx.inputElementConfig_elementConfig
        .  Rx.elementConfig_initialAttributes
        .~ mconcat
             [ "placeholder" =: "john.doe@email.com"
             , "class" =: "input"
             , "type" =: "email"
             ]
        )
    let btnAttrs = [("class", "button is-link"), ("type", "button")]
    (btn, _) <-
      Rx.element "button" (def & Rx.elementConfig_initialAttributes .~ btnAttrs)
        $ Rx.text "x"
    let dParticipant =
          Participant
            <$> Rx._inputElement_value wName
            <*> Rx._inputElement_value wEmail
    pure
      $ ( const (DeleteParticipant k) <$> Rx.domEvent Rx.Click btn
        , Rx.updated $ UpdateParticipant k <$> dParticipant
        )

buttonsWidget
  :: (Rx.DomBuilder t m, MonadFix m)
  => m (Rx.Event t AddParticipant, Rx.Event t Submit)
buttonsWidget = fieldHorizontal $ do
  label ""
  fieldBody . field' "is-grouped" $ do
    eNewParticipant <- control $ newParticipantWidget
    eSubmit         <- control $ submitWidget
    pure (eNewParticipant, eSubmit)

submitWidget :: Rx.DomBuilder t m => m (Rx.Event t Submit)
submitWidget = fieldHorizontal . control $ do
  let btnAttrs = [("class", "button is-primary"), ("type", "button")]
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
  -- dReqBody <- Rx.holdDyn (Left "") $ Right <$> eFormSubmitted
  -- eReqRes  <- echoForm dReqBody $ void eFormSubmitted
  let mkForm f = Rx.text . TL.toStrict . Pretty.pShowNoColor $ f
      -- displayErr = Rx.text
      -- displayReqRes rr = case rr of
      --   SR.ResponseSuccess _ f _ -> mkForm f
      --   SR.ResponseFailure _ t _ -> displayErr t
      --   SR.RequestFailure _ t    -> displayErr t
  -- Rx.widgetHold_ Rx.blank $ displayReqRes <$> eReqRes
  Rx.widgetHold_ Rx.blank $ mkForm <$> eFormSubmitted

field :: Rx.DomBuilder t m => m a -> m a
field = field' ""

fieldHorizontal :: Rx.DomBuilder t m => m a -> m a
fieldHorizontal = field' "is-horizontal"

field' :: Rx.DomBuilder t m => Text -> m a -> m a
field' c = Rx.elClass "div" $ "field " <> c

control :: Rx.DomBuilder t m => m a -> m a
control = control' ""

control' :: Rx.DomBuilder t m => Text -> m a -> m a
control' c = Rx.elClass "p" $ "control " <> c

label :: Rx.DomBuilder t m => Text -> m ()
label = Rx.elClass "div" "field-label" . Rx.elClass "label" "label" . Rx.text

fieldBody :: Rx.DomBuilder t m => m a -> m a
fieldBody = Rx.elClass "div" "field-body"
