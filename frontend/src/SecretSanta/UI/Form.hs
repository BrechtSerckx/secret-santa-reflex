{-# LANGUAGE DataKinds #-}
module SecretSanta.UI.Form
  ( formWidget
  , participantsWidget
  ) where

import           Control.Lens
import           Control.Monad.Fix
import           Data.Functor.Compose
import qualified Data.Map                      as Map
import           Data.Validate

import qualified Reflex                        as Rx
import qualified Reflex.Dom                    as Rx
import           Reflex.Dom                     ( (=:) )
import qualified Servant.Reflex                as SR

import           SecretSanta.Data

data Submit = Submit

formWidget :: forall t m . Rx.MonadWidget t m => m (Rx.Event t Form)
formWidget = do
  Rx.el "form" $ do
    rec
      -- General section
      title 3 $ Rx.text "General"

      -- Event name
      wEventName <- fieldHorizontal $ do
        label "Event"
        fieldBody . field . control $ eventNameWidget eSubmit

      -- Host name and email
      (wHostName, wHostEmail) <- fieldHorizontal $ do
        label "Organizer"
        fieldBody $ do
          wHostName'  <- field . control $ hostNameWidget eSubmit
          wHostEmail' <- field . control $ hostEmailWidget eSubmit
          pure (wHostName', wHostEmail')

      -- Event date and time
      (wDate, wTime) <- fieldHorizontal $ do
        label "Date/Time"
        fieldBody $ do
          wDate' <- field . control $ dateWidget eSubmit
          wTime' <- field . control $ timeWidget eSubmit
          pure (wDate', wTime')

      -- Location
      wLocation <- fieldHorizontal $ do
        label "Location"
        fieldBody . field . control $ locationWidget eSubmit

      -- Price
      wPrice <- fieldHorizontal $ do
        label "Price"
        fieldBody . field . control $ priceWidget eSubmit

      -- Description
      wDescription <- fieldHorizontal $ do
        label "Description"
        fieldBody . field . control $ descriptionWidget eSubmit

      -- Participant section
      title 3 $ Rx.text "Participants"
      wParticipants <- participantsWidget eNewParticipant
                                          layoutParticipant
                                          eSubmit

      -- Add participant and submit button
      (eNewParticipant, eSubmit) <- do
        label ""
        fieldBody . field' "is-grouped" $ do
          eNewParticipant' <- control $ newParticipantWidget
          eSubmit'         <- control $ submitWidget
          pure (eNewParticipant', eSubmit')

      -- Errors
      Rx.widgetHold_ Rx.blank . Rx.ffor eForm $ \case
        Failure es -> forM_ es $ Rx.elClass "p" "help is-danger" . Rx.text
        Success e  -> Rx.blank

      let
        -- TODO: ideally we would provide better feedback here.
        -- Currently, when there are non-unique participants or less than 3, we
        -- get an error message below the submit button, but the conflicting
        -- fields are still marked as valid.
        -- Ideally, we would throw the validation in a feedback loop where the
        -- conflicting fields are marked as invalid.
        -- We also would like to update the validation of all participants when
        -- one is updated?
        bForm = fmap (`bindValidation` validateForm) . getCompose $ do
          fEventName   <- Compose $ withFieldLabel "EventName" <$> wEventName
          fHostName    <- Compose $ withFieldLabel "Your name" <$> wHostName
          fHostEmail   <- Compose $ withFieldLabel "Your email" <$> wHostEmail
          fDate        <- Compose $ withFieldLabel "Date" <$> wDate
          fTime        <- Compose $ withFieldLabel "Time" <$> wTime
          fLocation    <- Compose $ withFieldLabel "Location" <$> wLocation
          fPrice       <- Compose $ withFieldLabel "Price" <$> wPrice
          fDescription <-
            Compose $ withFieldLabel "Description" <$> wDescription
          fParticipants <-
            Compose $ withFieldLabel "Participants" <$> wParticipants
          pure UnsafeForm { .. }
        eForm = Rx.tag bForm eSubmit
    pure . Rx.fforMaybe eForm $ \case
      Failure _ -> Nothing
      Success f -> Just f
 where
  withFieldLabel :: Text -> Validated a -> Validated a
  withFieldLabel t = first . fmap $ \e -> t <> ": " <> e
  layoutParticipant (wPName, wPEmail, wPDelete) = fieldHorizontal $ do
    label "Participant"
    fieldBody $ do
      wPName'   <- field . control $ wPName
      wPEmail'  <- field . control' "is-expanded" $ wPEmail
      wPDelete' <- control wPDelete
      pure (wPName', wPEmail', wPDelete')

eventNameWidget
  :: forall t m
   . Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated EventName))
eventNameWidget eSubmit = do
  let defaultAttrs = mconcat
        [ "placeholder" =: "My Secret Santa"
        , "class" =: "input"
        , "type" =: "text"
        ]
  rec let wUnvalidatedInput =
            Rx.inputElement
              $ def
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ defaultAttrs
                )
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ setValidationAttrs defaultAttrs
                )
      (setValidationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                            wUnvalidatedInput
                                                            validateEventName
  pure . Rx.current $ dValidatedInput

hostNameWidget
  :: forall t m
   . Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated PName))
hostNameWidget eSubmit = do
  let defaultAttrs = mconcat
        ["placeholder" =: "Your name", "class" =: "input", "type" =: "text"]
  rec let wUnvalidatedInput =
            Rx.inputElement
              $ def
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ defaultAttrs
                )
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ setValidationAttrs defaultAttrs
                )
      (setValidationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                            wUnvalidatedInput
                                                            validateHostName
  pure . Rx.current $ dValidatedInput

hostEmailWidget
  :: forall t m
   . Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated PEmail))
hostEmailWidget eSubmit = do
  let defaultAttrs = mconcat
        ["placeholder" =: "Your email", "class" =: "input", "type" =: "email"]
  rec let wUnvalidatedInput =
            Rx.inputElement
              $ def
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ defaultAttrs
                )
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ setValidationAttrs defaultAttrs
                )
      (setValidationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                            wUnvalidatedInput
                                                            validateHostEmail
  pure . Rx.current $ dValidatedInput

dateWidget
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated (Maybe Date)))
dateWidget eSubmit = do
  let defaultAttrs = mconcat ["class" =: "input", "type" =: "date"]
  rec let wUnvalidatedInput =
            Rx.inputElement
              $ def
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ defaultAttrs
                )
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ setValidationAttrs defaultAttrs
                )
      (setValidationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                            wUnvalidatedInput
                                                            validateDateMaybe
  pure . Rx.current $ dValidatedInput
 where

timeWidget
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated (Maybe Time)))
timeWidget eSubmit = do
  let defaultAttrs = mconcat ["class" =: "input", "type" =: "time"]
  rec let wUnvalidatedInput =
            Rx.inputElement
              $ def
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ defaultAttrs
                )
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ setValidationAttrs defaultAttrs
                )
      (setValidationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                            wUnvalidatedInput
                                                            validateTimeMaybe
  pure . Rx.current $ dValidatedInput

locationWidget
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated (Maybe Location)))
locationWidget eSubmit = do
  let defaultAttrs = mconcat
        [ "placeholder" =: "Location of the event"
        , "class" =: "input"
        , "type" =: "text"
        ]
  rec let wUnvalidatedInput =
            Rx.inputElement
              $ def
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ defaultAttrs
                )
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ setValidationAttrs defaultAttrs
                )
      (setValidationAttrs, dValidatedInput) <- mkValidation
        eSubmit
        wUnvalidatedInput
        validateLocationMaybe
  pure . Rx.current $ dValidatedInput

priceWidget
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated (Maybe Price)))
priceWidget eSubmit = do
  let defaultAttrs = mconcat
        [ "placeholder" =: "Price of the event"
        , "class" =: "input"
        , "type" =: "number"
        ]
  rec let wUnvalidatedInput =
            Rx.inputElement
              $ def
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ defaultAttrs
                )
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ setValidationAttrs defaultAttrs
                )
      (setValidationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                            wUnvalidatedInput
                                                            validatePriceMaybe
  pure . Rx.current $ dValidatedInput

descriptionWidget
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated Description))
descriptionWidget eSubmit = do
  let defaultAttrs = mconcat
        [ "placeholder" =: "Description of the event"
        , "class" =: "textarea"
        , "type" =: "text"
        , "rows" =: "5"
        ]
  rec let wUnvalidatedInput =
            Rx.textAreaElement
              $ def
              & (  Rx.textAreaElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ defaultAttrs
                )
              & (  Rx.textAreaElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ setValidationAttrs defaultAttrs
                )
      (setValidationAttrs, dValidatedInput) <- mkValidation
        eSubmit
        wUnvalidatedInput
        validateDescription
  pure . Rx.current $ dValidatedInput

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

type ParticipantMap = Map Int (Validated Participant)
initialParticipants :: ParticipantMap
initialParticipants = Map.fromList [ (i, emptyParticipant) | i <- [0 .. 2] ]

emptyParticipant :: Validated Participant
emptyParticipant = Failure . pure $ "empty participant"

addNewParticipant m = case Map.maxViewWithKey m of
  Nothing          -> [(0, emptyParticipant)]
  Just ((k, _), _) -> Map.insert (succ k) emptyParticipant m

type ParticipantLayout t m
  =  ( m (Rx.Dynamic t (Validated PName))
    , m (Rx.Dynamic t (Validated PEmail))
    , m (Rx.Event t DeleteParticipant)
    )
  -> m
       ( (Rx.Dynamic t (Validated PName))
       , (Rx.Dynamic t (Validated PEmail))
       , (Rx.Event t DeleteParticipant)
       )
participantsWidget
  :: Rx.MonadWidget t m
  => Rx.Event t AddParticipant
  -> ParticipantLayout t m
  -> Rx.Event t Submit
  -> m (Rx.Behavior t (Validated [Participant]))
participantsWidget eAddNewParticipant layout eSubmit = do
  rec
    dCurrParticipants <-
      Rx.foldDyn ($) initialParticipants
      . Rx.mergeWith (.)
      $ [ addNewParticipant <$ eAddNewParticipant
        , eDeleteParticipants
        , eUpdateParticipants
        ]
    dParticipantMap <- Rx.listWithKey dCurrParticipants
      $ \k p -> participantWidget k p layout eSubmit
    let eDeleteParticipants =
          overParticipants (\(DeleteParticipant i) -> Map.delete i)
            $   map fst
            <$> dParticipantMap
        eUpdateParticipants =
          overParticipants (\(UpdateParticipant i n) -> Map.adjust (const n) i)
            $   map snd
            <$> dParticipantMap
  pure . Rx.current $ sequenceA . Map.elems <$> dCurrParticipants
 where
  overParticipants f m =
    Rx.switch
      . Rx.current
      . Rx.ffor m
      $ Rx.mergeWith (.)
      . map (fmap f)
      . Map.elems

data DeleteParticipant = DeleteParticipant Int
data UpdateParticipant = UpdateParticipant Int (Validated Participant)

participantWidget
  :: forall t m
   . Rx.MonadWidget t m
  => Int
  -> Rx.Dynamic t (Validated Participant)
  -> ParticipantLayout t m
  -> Rx.Event t Submit
  -> m
       ( ( Rx.Event t DeleteParticipant
         , Rx.Event t UpdateParticipant
         )
       )
participantWidget k p layout eSubmit = do
  (bName, bEmail, eDeleted) <- layout
    (wPName eSubmit, wPEmail eSubmit, wPDelete k)
  let dParticipant :: Rx.Dynamic t (Validated Participant)
      dParticipant = getCompose $ do
        pName  <- Compose bName
        pEmail <- Compose bEmail
        pure Participant { .. }
      eUpdated = UpdateParticipant k <$> Rx.updated dParticipant
  pure $ (eDeleted, eUpdated)

wPName
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Dynamic t (Validated PName))
wPName eSubmit = do
  let defaultAttrs = mconcat
        [ "placeholder" =: "Participant name"
        , "class" =: "input"
        , "type" =: "text"
        ]
  rec let wUnvalidatedInput =
            Rx.inputElement
              $ def
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ defaultAttrs
                )
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ setValidationAttrs defaultAttrs
                )
      (setValidationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                            wUnvalidatedInput
                                                            validatePName
  pure dValidatedInput

wPEmail
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Dynamic t (Validated PEmail))
wPEmail eSubmit = do
  let defaultAttrs = mconcat
        [ "placeholder" =: "john.doe@email.com"
        , "class" =: "input"
        , "type" =: "email"
        ]
  rec let wUnvalidatedInput =
            Rx.inputElement
              $ def
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ defaultAttrs
                )
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ setValidationAttrs defaultAttrs
                )
      (setValidationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                            wUnvalidatedInput
                                                            validatePEmail
  pure dValidatedInput
wPDelete :: Rx.MonadWidget t m => Int -> m (Rx.Event t DeleteParticipant)
wPDelete k =
  fmap (fmap (const (DeleteParticipant k)) . Rx.domEvent Rx.Click)
    . fmap fst
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


mkValidation
  :: forall t m a elem val
   . ( Rx.MonadWidget t m
     , Rx.Value elem ~ Rx.Dynamic t val
     , Rx.DomEventType elem 'Rx.BlurTag ~ ()
     , Rx.HasDomEvent t elem 'Rx.BlurTag
     , Rx.HasValue elem
     )
  => Rx.Event t Submit
  -> m elem
  -> (val -> Validated a)
  -> m
       (  Map Rx.AttributeName Text
       -> Rx.Event t (Map Rx.AttributeName (Maybe Text))
       ,  Rx.Dynamic t (Validated a)
       )
mkValidation eSubmit wInput validate = do
  input <- wInput
  let eDoneEditing = Rx.leftmost [void eSubmit, Rx.domEvent Rx.Blur input]
      dValue       = fmap validate . Rx.value $ input
      bValue       = Rx.current dValue
      eValue       = Rx.tag bValue eDoneEditing
  Rx.widgetHold_ Rx.blank . Rx.ffor eValue $ \case
    Failure es -> forM_ es $ Rx.elClass "p" "help is-danger" . Rx.text
    Success _  -> Rx.blank
  let eModifyAttrs defAttrs = Rx.ffor eValue $ \case
        Failure _ -> modifyClass defAttrs "is-danger"
        Success _ -> modifyClass defAttrs "is-success"
  pure (eModifyAttrs, dValue)
 where
  modifyClass
    :: Map Rx.AttributeName Text -> Text -> Map Rx.AttributeName (Maybe Text)
  modifyClass defAttrs style =
    let newClass = Just $ case Map.lookup "class" defAttrs of
          Nothing -> style
          Just c  -> c <> " " <> style
    in  [("class", newClass)]
