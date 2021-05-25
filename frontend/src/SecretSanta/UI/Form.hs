{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
module SecretSanta.UI.Form
  ( formWidget
  , participantsWidget
  )
where

import           Control.Lens
import           Control.Monad.Fix
import           Data.Functor.Compose
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import "common"  Data.Time
import           Data.Time.MonadTime
import           Data.Validate

import qualified Reflex                        as Rx
import qualified Reflex.Dom                    as Rx
import           Reflex.Dom                     ( (=:) )

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
      bEventName <- fieldHorizontal $ do
        label "Event*"
        fieldBody . field . control $ eventNameWidget eSubmit

      -- Host name and email
      (bHostName, bHostEmail) <- fieldHorizontal $ do
        label "Organizer*"
        fieldBody $ do
          wHostName'  <- field . control $ hostNameWidget eSubmit
          wHostEmail' <- field . control $ hostEmailWidget eSubmit
          pure (wHostName', wHostEmail')

      -- Event date and time
      (bDate, bTime) <- fieldHorizontal $ do
        label "Date/Time"
        fieldBody $ do
          wDate' <- field . control $ dateWidget eSubmit
          wTime' <- field . control $ timeWidget eSubmit
          pure (wDate', wTime')
      fTimeZone  <- liftIO getTimeZone
      clientTime <- liftIO getZonedTime
      let bDateTimeErrs =
            fmap (`bindValidation` identity)
              .   getCompose
              $   validateDateTime clientTime fTimeZone
              <$> Compose bDate
              <*> Compose bTime
          eDateTimeErrs = getFailures <$> Rx.tag bDateTimeErrs eSubmit


      -- Location
      bLocation <- fieldHorizontal $ do
        label "Location"
        fieldBody . field . control $ locationWidget eSubmit

      -- Price
      bPrice <- fieldHorizontal $ do
        label "Price"
        fieldBody . field . control $ priceWidget eSubmit

      -- Description
      bDescription <- fieldHorizontal $ do
        label "Description*"
        fieldBody . field . control $ descriptionWidget eSubmit

      -- Participant section
      title 3 $ Rx.text "Participants"
      bParticipants <- participantsWidget eNewParticipant
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
      Rx.widgetHold_ Rx.blank
        . Rx.ffor (Rx.mergeWith (<>) [getFailures <$> eForm, eDateTimeErrs])
        $ \case
            [] -> Rx.blank
            es -> forM_ es $ Rx.elClass "p" "help is-danger" . Rx.text

      let
        -- TODO: ideally we would provide better feedback here.
        -- Currently, when there are less than 3 participants, we
        -- get an error message below the submit button, but we can submit.
        -- Ideally we should disable the submit button
        bForm = fmap (`bindValidation` validateForm) . getCompose $ do
          fEventName   <- Compose $ withFieldLabel "Event name" <$> bEventName
          fHostName    <- Compose $ withFieldLabel "Your name" <$> bHostName
          fHostEmail   <- Compose $ withFieldLabel "Your email" <$> bHostEmail
          fDate        <- Compose $ withFieldLabel "Date" <$> bDate
          fTime        <- Compose $ withFieldLabel "Time" <$> bTime
          fLocation    <- Compose $ withFieldLabel "Location" <$> bLocation
          fPrice       <- Compose $ withFieldLabel "Price" <$> bPrice
          fDescription <-
            Compose $ withFieldLabel "Description" <$> bDescription
          fParticipants <-
            Compose $ withFieldLabel "Participants" <$> bParticipants
          pure UnsafeForm { .. }
        eForm = Rx.tag bForm eSubmit
    pure . Rx.fforMaybe eForm $ \case
      Failure _ -> Nothing
      Success f -> Just f
 where
  withFieldLabel :: Text -> Validated a -> Validated a
  withFieldLabel t = first . fmap $ \e -> t <> ": " <> e
  layoutParticipant (wPName', wPEmail', wPDelete') = fieldHorizontal $ do
    label "Participant*"
    fieldBody $ do
      wPName''   <- field . control $ wPName'
      wPEmail''  <- field . control' "is-expanded" $ wPEmail'
      wPDelete'' <- control wPDelete'
      pure (wPName'', wPEmail'', wPDelete'')

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
        , "required" =: "required"
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
      (setValidationAttrs, dValidatedInput) <- mkSimpleValidation
        eSubmit
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
        [ "placeholder" =: "Your name"
        , "class" =: "input"
        , "type" =: "text"
        , "required" =: "required"
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
      (setValidationAttrs, dValidatedInput) <- mkSimpleValidation
        eSubmit
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
        [ "placeholder" =: "Your email"
        , "class" =: "input"
        , "type" =: "email"
        , "required" =: "required"
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
      (setValidationAttrs, dValidatedInput) <- mkSimpleValidation
        eSubmit
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
      (setValidationAttrs, dValidatedInput) <- mkSimpleValidation
        eSubmit
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
      (setValidationAttrs, dValidatedInput) <- mkSimpleValidation
        eSubmit
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
      (setValidationAttrs, dValidatedInput) <- mkSimpleValidation
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
      (setValidationAttrs, dValidatedInput) <- mkSimpleValidation
        eSubmit
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
        , "required" =: "required"
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
      (setValidationAttrs, dValidatedInput) <- mkSimpleValidation
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

addNewParticipant :: ParticipantMap -> ParticipantMap
addNewParticipant m = case Map.maxViewWithKey m of
  Nothing          -> [(0, emptyParticipant)]
  Just ((k, _), _) -> Map.insert (succ k) emptyParticipant m

updateParticipantName
  :: Int -> Validated PName -> ParticipantMap -> ParticipantMap
updateParticipantName i name = Map.adjust (\(_name, email) -> (name, email)) i
updateParticipantEmail
  :: Int -> Validated PEmail -> ParticipantMap -> ParticipantMap
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
      pEvents <- Rx.listWithKey dParticipantMap $ \k _p -> do
        let dPMap' = Map.elems . Map.delete k <$> dParticipantMap
        (dName, dEmail, eDeleted) <- layout
          ( wPName eSubmit $ map fst <$> dPMap'
          , wPEmail eSubmit $ map snd <$> dPMap'
          , wPDelete k
          )
        let eUpdatePName  = UpdatePName k <$> Rx.updated dName
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
        , "required" =: "required"
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
        , "required" =: "required"
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


mkSimpleValidation
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
mkSimpleValidation eSubmit wInput validate = do
  let construct' = fmap validate . Rx.value
      validate' e d =
        Rx.tagPromptlyDyn d (Rx.leftmost [e, void eSubmit]) <&> \case
          Success _  -> []
          Failure es -> es
  (eValidationAttrs, dRes) <- mkValidation wInput construct' validate'
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
        mkGlobalErrs (Failure _es) _      = []
        mkGlobalErrs (Success a  ) global = validateGlobal a global
        eGlobalErrs =
          Rx.tagPromptlyDyn (mkGlobalErrs <$> d <*> dGlobal) $ Rx.leftmost
            [ void eSubmit
            , eDoneEditing
            , Rx.gate (hasFailures <$> Rx.current d) eGlobal
            ]
      in
        Rx.mergeWith (<>) [eLocalErrs, eGlobalErrs]
  (eValidationAttrs, dRes) <- mkValidation wInput construct validate
  pure (eValidationAttrs, dRes)

modifyClass
  :: Map Rx.AttributeName Text -> [Text] -> Map Rx.AttributeName (Maybe Text)
modifyClass defAttrs style =
  let newClass = Just $ case Map.lookup "class" defAttrs of
        Nothing -> T.unwords style
        Just c  -> c <> " " <> T.unwords style
  in  [("class", newClass)]

mkValidation
  :: forall t m elem res
   . ( Rx.MonadWidget t m
     , Rx.HasDomEvent t elem 'Rx.BlurTag
     , Rx.DomEventType elem 'Rx.BlurTag ~ ()
     )
  => m elem -- ^ the widget
  -> (elem -> Rx.Dynamic t res) -- ^ constructs the resulting value from the widget
  -> (Rx.Event t () -> Rx.Dynamic t res -> Rx.Event t [Text]) -- ^ validates the widget value on an event
  -> m (Rx.Event t [Text], Rx.Dynamic t res)
mkValidation wInput construct validate = do
  input <- wInput
  let eOnChange =
        Rx.leftmost [void $ Rx.updated dRes, Rx.domEvent Rx.Blur input]
      dRes  = construct input
      eErrs = validate eOnChange dRes
  Rx.widgetHold_ Rx.blank . Rx.ffor eErrs $ \case
    [] -> Rx.blank
    es -> forM_ es $ Rx.elClass "p" "help is-danger" . Rx.text
  let eValidationAttrs = Rx.ffor eErrs $ \case
        [] -> pure "is-success"
        _  -> pure "is-danger"
  pure (eValidationAttrs, dRes)
