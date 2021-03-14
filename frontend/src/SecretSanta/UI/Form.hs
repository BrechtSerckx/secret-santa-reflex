{-# LANGUAGE DataKinds #-}
module SecretSanta.UI.Form
  ( formWidget
  , participantsWidget
  ) where

import           Control.Lens
import           Control.Monad.Fix
import           Data.Functor.Compose
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           Data.Validate

import qualified Reflex                        as Rx
import qualified Reflex.Dom                    as Rx
import           Reflex.Dom                     ( (=:) )
import qualified Servant.Reflex                as SR

import           SecretSanta.Data

data Submit = Submit
  deriving (Eq, Show)

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

type ParticipantMap = Map Int Participant'
type Participant' = (Validated PName, Validated PEmail)
initialParticipants :: ParticipantMap
initialParticipants = Map.fromList [ (i, emptyParticipant) | i <- [0 .. 2] ]

emptyParticipant :: Participant'
emptyParticipant =
  (Failure . pure $ "empty participant", Failure . pure $ "empty participant")

addNewParticipant m = case Map.maxViewWithKey m of
  Nothing          -> [(0, emptyParticipant)]
  Just ((k, _), _) -> Map.insert (succ k) emptyParticipant m
updateParticipantName i name = Map.adjust (\(_name, email) -> (name, email)) i
updateParticipantEmail i email =
  Map.adjust (\(name, _email) -> (name, email)) i

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
  :: forall t m
   . Rx.MonadWidget t m
  => Rx.Event t AddParticipant
  -> ParticipantLayout t m
  -> Rx.Event t Submit
  -> m (Rx.Behavior t (Validated [Participant]))
participantsWidget eAddNewParticipant layout eSubmit = do
  rec dParticipantMap :: Rx.Dynamic t ParticipantMap <-
        Rx.foldDyn ($) initialParticipants
        . Rx.mergeWith (.)
        $ [ addNewParticipant <$ eAddNewParticipant
          , eDeleteParticipants pEvents
          , eUpdatePNames pEvents
          , eUpdatePEmails pEvents
          ]
      pEvents <- Rx.listWithKey dParticipantMap $ \k p -> do
        let dPMap' = Map.elems . Map.delete k <$> dParticipantMap
        (dName, dEmail, eDeleted) <- layout
          ( wPName eSubmit $ map fst <$> dPMap'
          , wPEmail eSubmit $ map snd <$> dPMap'
          , wPDelete k
          )
        let dParticipant :: Rx.Dynamic t (Validated Participant)
            dParticipant = getCompose $ do
              pName  <- Compose dName
              pEmail <- Compose dEmail
              pure Participant { .. }
            eUpdatePName  = UpdatePName k <$> Rx.updated dName
            eUpdatePEmail = UpdatePEmail k <$> Rx.updated dEmail
        pure $ (eDeleted, eUpdatePName, eUpdatePEmail)
  pure
    $   sequenceA
    .   map mkParticipant
    .   Map.elems
    <$> Rx.current dParticipantMap
 where
  overParticipants f m =
    Rx.switch
      . Rx.current
      . Rx.ffor m
      $ Rx.mergeWith (.)
      . map (fmap f)
      . Map.elems
  eDeleteParticipants dParticipantMap =
    overParticipants (\(DeleteParticipant i) -> Map.delete i)
      $   map fst3
      <$> dParticipantMap
  eUpdatePNames dParticipantMap =
    overParticipants (\(UpdatePName i name) -> updateParticipantName i name)
      $   map snd3
      <$> dParticipantMap
  eUpdatePEmails dParticipantMap =
    overParticipants (\(UpdatePEmail i email) -> updateParticipantEmail i email)
      $   map thd3
      <$> dParticipantMap
  mkParticipant :: Participant' -> Validated Participant
  mkParticipant (pName, pEmail) = Participant <$> pName <*> pEmail

data DeleteParticipant = DeleteParticipant Int
data UpdatePName = UpdatePName Int (Validated PName)
data UpdatePEmail = UpdatePEmail Int (Validated PEmail)

wPName
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> Rx.Dynamic t [Validated PName]
  -> m (Rx.Dynamic t (Validated PName))
wPName eSubmit dPNames = do
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
                .~ (modifyClass defaultAttrs <$> eValidationAttrs)
                )
      (eValidationAttrs, dValidatedInput) <- mkPValidation wUnvalidatedInput
                                                           eSubmit
                                                           validatePName
                                                           validatePNameUnique
                                                           dPNames
  pure dValidatedInput

wPEmail
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> Rx.Dynamic t [Validated PEmail]
  -> m (Rx.Dynamic t (Validated PEmail))
wPEmail eSubmit dPEmails = do
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
                .~ (modifyClass defaultAttrs <$> eValidationAttrs)
                )
      (eValidationAttrs, dValidatedInput) <- mkPValidation
        wUnvalidatedInput
        eSubmit
        validatePEmail
        validatePEmailUnique
        dPEmails
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
  let construct' = fmap validate . Rx.value
      validate' e d =
        Rx.tagPromptlyDyn d (Rx.leftmost [e, void eSubmit]) <&> \case
          Success _  -> []
          Failure es -> es
  (eValidationAttrs, dRes) <- mkValidation' wInput construct' validate'
  let eModifyAttrs defAttrs = modifyClass defAttrs <$> eValidationAttrs
  pure (eModifyAttrs, dRes)

mkPValidation
  :: forall t m a elem val
   . ( Rx.MonadWidget t m
     , Rx.Value elem ~ Rx.Dynamic t val
     , Rx.DomEventType elem 'Rx.BlurTag ~ ()
     , Rx.HasDomEvent t elem 'Rx.BlurTag
     , Rx.HasValue elem
     , Show a
     )
  => m elem
  -> Rx.Event t Submit
  -> (val -> Validated a)
  -> (a -> [Validated a] -> [Text])
  -> Rx.Dynamic t [Validated a]
  -> m (Rx.Event t [Text], Rx.Dynamic t (Validated a))
mkPValidation wInput eSubmit validateLocal validateGlobal dGlobal = do
  let
    eGlobal   = void $ Rx.updated dGlobal
    construct = fmap validateLocal . Rx.value
    validate eDoneEditing d =
      let
        eLocalErrs = Rx.tagPromptlyDyn (getFailures <$> d)
          $ Rx.leftmost [void eSubmit, eDoneEditing]
        mkGlobalErrs (Failure es) _      = []
        mkGlobalErrs (Success a ) global = validateGlobal a global
        eGlobalErrs =
          Rx.tagPromptlyDyn (mkGlobalErrs <$> d <*> dGlobal) $ Rx.leftmost
            [ void eSubmit
            , eDoneEditing
            , Rx.gate (hasFailures <$> Rx.current d) eGlobal
            ]
      in
        Rx.mergeWith (<>) [eLocalErrs, eGlobalErrs]
  (eValidationAttrs, dRes) <- mkValidation' wInput construct validate
  pure (eValidationAttrs, dRes)

modifyClass
  :: Map Rx.AttributeName Text -> [Text] -> Map Rx.AttributeName (Maybe Text)
modifyClass defAttrs style =
  let newClass = Just $ case Map.lookup "class" defAttrs of
        Nothing -> T.unwords style
        Just c  -> c <> " " <> T.unwords style
  in  [("class", newClass)]

mkValidation'
  :: forall t m elem res rest
   . ( Rx.MonadWidget t m
     , Rx.HasDomEvent t elem 'Rx.BlurTag
     , Rx.DomEventType elem 'Rx.BlurTag ~ ()
     )
  => m elem -- ^ the widget
  -> (elem -> Rx.Dynamic t res) -- ^ constructs the resulting value from the widget
  -> (Rx.Event t () -> Rx.Dynamic t res -> Rx.Event t [Text]) -- ^ validates the widget value on an event
  -> m (Rx.Event t [Text], Rx.Dynamic t res)
mkValidation' wInput construct validate = do
  input <- wInput
  let eDoneEditing = Rx.domEvent Rx.Blur input
      dRes         = construct input
      eErrs        = validate eDoneEditing dRes
  Rx.widgetHold_ Rx.blank . Rx.ffor eErrs $ \case
    [] -> Rx.blank
    es -> forM_ es $ Rx.elClass "p" "help is-danger" . Rx.text
  let eValidationAttrs = Rx.ffor eErrs $ \case
        [] -> pure "is-success"
        _  -> pure "is-danger"
  pure (eValidationAttrs, dRes)
