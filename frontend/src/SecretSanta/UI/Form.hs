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
  :: forall t m
   . (Rx.DomBuilder t m, MonadFix m, Rx.MonadHold t m, Rx.PostBuild t m)
  => m (Rx.Event t Form)
formWidget = do
  Rx.el "form" $ do
    rec
      title 3 $ Rx.text "General"
      wName <- fieldHorizontal $ do
        label "Event"
        fieldBody . field . control $ nameWidget eSubmit
      (wDate, wTime) <- fieldHorizontal $ do
        label "Date/Time"
        fieldBody $ do
          wDate' <- field . control $ dateWidget
          wTime' <- field . control $ timeWidget
          pure (wDate', wTime')
      wLocation <- fieldHorizontal $ do
        label "Location"
        fieldBody . field . control $ locationWidget
      wPrice <- fieldHorizontal $ do
        label "Price"
        fieldBody . field . control $ priceWidget
      wDescription <- fieldHorizontal $ do
        label "Description"
        fieldBody . field . control $ descriptionWidget
      title 3 $ Rx.text "Participants"
      rec wParticipants <-
            participantsWidget eNewParticipant $ layoutParticipant
          (eNewParticipant, eSubmit) <- do
            label ""
            fieldBody . field' "is-grouped" $ do
              eNewParticipant' <- control $ newParticipantWidget
              eSubmit'         <- control $ submitWidget
              pure (eNewParticipant', eSubmit')
    let submit = do
          fName         <- wName
          fDate         <- wDate
          fTime         <- wTime
          fLocation     <- wLocation
          fPrice        <- wPrice
          fDescription  <- wDescription
          fParticipants <- Map.elems <$> wParticipants
          pure Form { .. }
    pure . Rx.tag submit $ eSubmit
 where
  layoutParticipant (wPName, wPEmail, wPDelete) = fieldHorizontal $ do
    label "Participant"
    fieldBody $ do
      wPName'   <- field . control $ wPName
      wPEmail'  <- field . control' "is-expanded" $ wPEmail
      wPDelete' <- control wPDelete
      pure (wPName', wPEmail', wPDelete')

nameWidget
  :: (Rx.DomBuilder t m, MonadFix m, Rx.MonadHold t m)
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated Text))
nameWidget eSubmit = do
  rec let (validationAttrs, validationMsg, validatedValue) =
            mkValidation eSubmit input validate
      input <-
        Rx.inputElement
        $ def
        & (  Rx.inputElementConfig_elementConfig
          .  Rx.elementConfig_initialAttributes
          .~ mconcat
               [ "placeholder" =: "My Secret Santa"
               , "class" =: "input"
               , "type" =: "text"
               ]
          )
        & (  Rx.inputElementConfig_elementConfig
          .  Rx.elementConfig_modifyAttributes
          .~ validationAttrs
          )
      validationMsg
  pure validatedValue
 where
  validate text =
    if T.null text then Left "Name cannot be empty" else Right text

type Validated a = Either Text a
mkValidation
  :: (Rx.DomBuilder t m, MonadFix m, Rx.MonadHold t m)
  => Rx.Event t Submit
  -> Rx.InputElement Rx.EventResult (Rx.DomBuilderSpace m) t
  -> (Text -> Validated a)
  -> ( Rx.Event t (Map Rx.AttributeName (Maybe Text))
     , m ()
     , Rx.Behavior t (Validated a)
     )
mkValidation eSubmit input validate =
  let eDoneEditing = Rx.leftmost [void eSubmit, Rx.domEvent Rx.Blur input]
      eModifyAttrs = Rx.ffor eValue $ \case
        Left  _ -> [("class", Just "input is-danger")]
        Right _ -> [("class", Just "input is-success")]
      dValue  = fmap validate . Rx.current . Rx._inputElement_value $ input
      eValue  = Rx.tag dValue eDoneEditing
      wErrMsg = Rx.widgetHold_ Rx.blank . Rx.ffor eValue $ \case
        Left  e -> Rx.elClass "p" "help is-danger" $ Rx.text e
        Right e -> Rx.blank
  in  (eModifyAttrs, wErrMsg, dValue)

dateWidget :: Rx.DomBuilder t m => m (Rx.Behavior t Text)
dateWidget =
  fmap (fmap mkDate . Rx.current . Rx._inputElement_value)
    . Rx.inputElement
    $ def
    & (  Rx.inputElementConfig_elementConfig
      .  Rx.elementConfig_initialAttributes
      .~ mconcat ["class" =: "input", "type" =: "date"]
      )
  where
  -- TODO
        mkDate = identity

timeWidget :: Rx.DomBuilder t m => m (Rx.Behavior t Text)
timeWidget =
  fmap (fmap mkDate . Rx.current . Rx._inputElement_value)
    . Rx.inputElement
    $ def
    & (  Rx.inputElementConfig_elementConfig
      .  Rx.elementConfig_initialAttributes
      .~ mconcat ["class" =: "input", "type" =: "time"]
      )
  where
    -- TODO
        mkDate = identity

locationWidget :: Rx.DomBuilder t m => m (Rx.Behavior t Text)
locationWidget =
  fmap (Rx.current . Rx._inputElement_value)
    . Rx.inputElement
    $ def
    & (  Rx.inputElementConfig_elementConfig
      .  Rx.elementConfig_initialAttributes
      .~ mconcat
           [ "placeholder" =: "Location of the event"
           , "class" =: "input"
           , "type" =: "text"
           ]
      )

priceWidget :: Rx.DomBuilder t m => m (Rx.Behavior t Double)
priceWidget =
  fmap (fmap mkPrice . Rx.current . Rx._inputElement_value)
    . Rx.inputElement
    $ def
    & (  Rx.inputElementConfig_elementConfig
      .  Rx.elementConfig_initialAttributes
      .~ mconcat
           [ "placeholder" =: "Price of the event"
           , "class" =: "input"
           , "type" =: "number"
           ]
      )
        -- TODO
  where mkPrice = fromMaybe 0 . readMaybe . T.unpack

descriptionWidget :: Rx.DomBuilder t m => m (Rx.Behavior t Text)
descriptionWidget =
  fmap (Rx.current . Rx._inputElement_value)
    . Rx.inputElement
    $ def
    & (  Rx.inputElementConfig_elementConfig
      .  Rx.elementConfig_initialAttributes
      .~ mconcat
           [ "placeholder" =: "Description of the event"
           , "class" =: "input"
           , "type" =: "text"
           ]
      )

data AddParticipant = AddParticipant

newParticipantWidget
  :: (Rx.DomBuilder t m, MonadFix m) => m (Rx.Event t AddParticipant)
newParticipantWidget = fieldHorizontal . control $ do
  (e, _) <-
    Rx.element
        "button"
        (  def
        &  Rx.elementConfig_initialAttributes
        .~ [("class", "button is-info"), ("type", "button")]
        )
      $ Rx.text "Add Participant"
  pure $ Rx.domEvent Rx.Click e $> AddParticipant

type ParticipantMap = Map Int Participant
initialParticipants :: ParticipantMap
initialParticipants = Map.fromList [ (i, emptyParticipant) | i <- [0 .. 2] ]
emptyParticipant :: Participant
emptyParticipant = Participant "" ""
addNewParticipant m = case Map.maxViewWithKey m of
  Nothing          -> [(0, emptyParticipant)]
  Just ((k, _), _) -> Map.insert (succ k) emptyParticipant m

participantsWidget
  :: (Rx.DomBuilder t m, Rx.MonadHold t m, MonadFix m, Rx.PostBuild t m)
  => Rx.Event t AddParticipant
  -> ParticipantLayout t m
  -> m (Rx.Behavior t ParticipantMap)
participantsWidget eAddNewParticipant layout = do
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
      $ \k p -> participantWidget k p layout
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

type ParticipantLayout t m
  =  ( m (Rx.InputElement Rx.EventResult (Rx.DomBuilderSpace m) t)
    , m (Rx.InputElement Rx.EventResult (Rx.DomBuilderSpace m) t)
    , m (Rx.Element Rx.EventResult (Rx.DomBuilderSpace m) t)
    )
  -> m
       ( Rx.InputElement Rx.EventResult (Rx.DomBuilderSpace m) t
       , Rx.InputElement Rx.EventResult (Rx.DomBuilderSpace m) t
       , Rx.Element Rx.EventResult (Rx.DomBuilderSpace m) t
       )
participantWidget
  :: (Rx.DomBuilder t m, Rx.PostBuild t m)
  => Int
  -> Rx.Dynamic t Participant
  -> ParticipantLayout t m
  -> m
       ( ( Rx.Event t DeleteParticipant
         , Rx.Event t UpdateParticipant
         )
       )
participantWidget k p layout = do
  (wName', wEmail', wDelete') <- layout (wName, wEmail, wDelete)
  let dParticipant =
        Participant
          <$> Rx._inputElement_value wName'
          <*> Rx._inputElement_value wEmail'
  pure
    $ ( const (DeleteParticipant k) <$> Rx.domEvent Rx.Click wDelete'
      , Rx.updated $ UpdateParticipant k <$> dParticipant
      )
 where
  wName =
    Rx.inputElement
      $ def
      & (  Rx.inputElementConfig_elementConfig
        .  Rx.elementConfig_initialAttributes
        .~ mconcat
             [ "placeholder" =: "Participant name"
             , "class" =: "input"
             , "type" =: "text"
             ]
        )
  wEmail =
    Rx.inputElement
      $ def
      & (  Rx.inputElementConfig_elementConfig
        .  Rx.elementConfig_initialAttributes
        .~ mconcat
             [ "placeholder" =: "john.doe@email.com"
             , "class" =: "input"
             , "type" =: "email"
             ]
        )
  wDelete =
    fmap fst
      . Rx.element
          "button"
          (  def
          &  Rx.elementConfig_initialAttributes
          .~ [("class", "button is-danger"), ("type", "button")]
          )
      $ Rx.text "x"

submitWidget :: Rx.DomBuilder t m => m (Rx.Event t Submit)
submitWidget = fieldHorizontal . control $ do
  (e, _) <-
    Rx.element
        "button"
        (  def
        &  Rx.elementConfig_initialAttributes
        .~ [("class", "button is-success"), ("type", "button")]
        )
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

title :: Rx.DomBuilder t m => Int -> m a -> m a
title (show -> i) = Rx.elClass ("h" <> i) ("title is-" <> i)

