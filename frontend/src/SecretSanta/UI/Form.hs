module SecretSanta.UI.Form
  ( formWidget
  , formDisplayWidget
  , participantsWidget
  ) where

import           Control.Lens
import           Control.Monad.Fix
import           Data.Either.Validation
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Data.Time                      ( Day
                                                , TimeOfDay
                                                , makeTimeOfDayValid
                                                )
import qualified Text.Pretty.Simple            as Pretty

import qualified Reflex                        as Rx
import qualified Reflex.Dom                    as Rx
import           Reflex.Dom                     ( (=:) )
import qualified Servant.Reflex                as SR

import           SecretSanta.Data

data Submit = Submit

formWidget :: forall t m . Rx.MonadWidget t m => m (Rx.Event t (Maybe Form))
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
          wDate' <- field . control $ dateWidget eSubmit
          wTime' <- field . control $ timeWidget eSubmit
          pure (wDate', wTime')
      wLocation <- fieldHorizontal $ do
        label "Location"
        fieldBody . field . control $ locationWidget eSubmit
      wPrice <- fieldHorizontal $ do
        label "Price"
        fieldBody . field . control $ priceWidget eSubmit
      wDescription <- fieldHorizontal $ do
        label "Description"
        fieldBody . field . control $ descriptionWidget eSubmit
      title 3 $ Rx.text "Participants"
      rec wParticipants <- participantsWidget eNewParticipant
                                              layoutParticipant
                                              eSubmit
          (eNewParticipant, eSubmit) <- do
            label ""
            fieldBody . field' "is-grouped" $ do
              eNewParticipant' <- control $ newParticipantWidget
              eSubmit'         <- control $ submitWidget
              pure (eNewParticipant', eSubmit')
      Rx.widgetHold_ Rx.blank . Rx.ffor eForm $ \case
        Failure es -> forM_ es $ Rx.elClass "p" "help is-danger" . Rx.text
        Success e  -> Rx.blank
      let
        submit = do
          eName         <- wName
          eDate         <- wDate
          eTime         <- wTime
          eLocation     <- wLocation
          ePrice        <- wPrice
          eDescription  <- wDescription
          eParticipants <- Map.elems <$> wParticipants
          pure $ do
            let withFieldLabel :: Text -> Validated a -> Validated a
                withFieldLabel t = first . fmap $ \e -> t <> ": " <> e
            fName         <- withFieldLabel "Event" eName
            fDate         <- withFieldLabel "Date" eDate
            fTime         <- withFieldLabel "Time" eTime
            fLocation     <- withFieldLabel "Location" eLocation
            fPrice        <- withFieldLabel "Price" ePrice
            fDescription  <- withFieldLabel "Description" eDescription
            fParticipants <-
              withFieldLabel "Participants" . sequenceA $ eParticipants
            pure Form { .. }
        eForm = Rx.tag submit $ eSubmit
    pure
      $   (\case
            Failure _ -> Nothing
            Success f -> Just f
          )
      <$> eForm
 where
  layoutParticipant (wPName, wPEmail, wPDelete) = fieldHorizontal $ do
    label "Participant"
    fieldBody $ do
      wPName'   <- field . control $ wPName
      wPEmail'  <- field . control' "is-expanded" $ wPEmail
      wPDelete' <- control wPDelete
      pure (wPName', wPEmail', wPDelete')

nameWidget
  :: forall t m
   . Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated Name))
nameWidget eSubmit = do
  rec let wUnvalidatedInput =
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
      (validationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                         wUnvalidatedInput
                                                         validateName
  pure . Rx.current $ dValidatedInput
 where

dateWidget
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated (Maybe Day)))
dateWidget eSubmit = do
  rec let wUnvalidatedInput =
            Rx.inputElement
              $ def
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ mconcat ["class" =: "input", "type" =: "date"]
                )
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ validationAttrs
                )
      (validationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                         wUnvalidatedInput
                                                         validateDate
  pure . Rx.current $ dValidatedInput
 where

timeWidget
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated (Maybe TimeOfDay)))
timeWidget eSubmit = do
  rec let wUnvalidatedInput =
            Rx.inputElement
              $ def
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ mconcat ["class" =: "input", "type" =: "time"]
                )
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ validationAttrs
                )
      (validationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                         wUnvalidatedInput
                                                         validateTime
  pure . Rx.current $ dValidatedInput

locationWidget
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated (Maybe Location)))
locationWidget eSubmit = do
  rec let wUnvalidatedInput =
            Rx.inputElement
              $ def
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ mconcat
                     [ "placeholder" =: "Location of the event"
                     , "class" =: "input"
                     , "type" =: "text"
                     ]
                )
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ validationAttrs
                )
      (validationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                         wUnvalidatedInput
                                                         validateLocation
  pure . Rx.current $ dValidatedInput

priceWidget
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated (Maybe Price)))
priceWidget eSubmit = do
  rec let wUnvalidatedInput =
            Rx.inputElement
              $ def
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ mconcat
                     [ "placeholder" =: "Price of the event"
                     , "class" =: "input"
                     , "type" =: "number"
                     ]
                )
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ validationAttrs
                )
      (validationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                         wUnvalidatedInput
                                                         validatePrice
  pure . Rx.current $ dValidatedInput

descriptionWidget
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.Behavior t (Validated Description))
descriptionWidget eSubmit = do
  rec let wUnvalidatedInput =
            Rx.inputElement
              $ def
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_initialAttributes
                .~ mconcat
                     [ "placeholder" =: "Description of the event"
                     , "class" =: "input"
                     , "type" =: "text"
                     ]
                )
              & (  Rx.inputElementConfig_elementConfig
                .  Rx.elementConfig_modifyAttributes
                .~ validationAttrs
                )
      (validationAttrs, dValidatedInput) <- mkValidation eSubmit
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
  -> m (Rx.Behavior t ParticipantMap)
participantsWidget eAddNewParticipant layout eSubmit = do
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
      $ \k p -> participantWidget k p layout eSubmit
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
  (bName, bEmail, eDeleted) <- layout (wName, wEmail, wDelete)
  let dParticipant :: Rx.Dynamic t (Validated Participant)
      dParticipant = do
        eName  <- bName
        eEmail <- bEmail
        pure $ do
          pName  <- eName
          pEmail <- eEmail
          pure Participant { .. }
      eUpdated = UpdateParticipant k <$> Rx.updated dParticipant
  pure $ (eDeleted, eUpdated)
 where
  wName :: m (Rx.Dynamic t (Validated PName))
  wName = do
    rec let wUnvalidatedInput =
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
                & (  Rx.inputElementConfig_elementConfig
                  .  Rx.elementConfig_modifyAttributes
                  .~ validationAttrs
                  )
        (validationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                           wUnvalidatedInput
                                                           validatePName
    pure dValidatedInput
  wEmail :: m (Rx.Dynamic t (Validated PEmail))
  wEmail = do
    rec let wUnvalidatedInput =
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
                & (  Rx.inputElementConfig_elementConfig
                  .  Rx.elementConfig_modifyAttributes
                  .~ validationAttrs
                  )
        (validationAttrs, dValidatedInput) <- mkValidation eSubmit
                                                           wUnvalidatedInput
                                                           validatePEmail
    pure dValidatedInput
  wDelete =
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

formDisplayWidget
  :: forall t m
   . (Rx.DomBuilder t m, Rx.MonadHold t m, Rx.PostBuild t m)
  => Rx.Event t (Maybe Form)
  -> (  Rx.Dynamic t (Either Text Form)
     -> Rx.Event t ()
     -> m (Rx.Event t (SR.ReqResult () Form))
     )
  -> m ()
formDisplayWidget eFormSubmitted echoForm = do
  -- dReqBody <- Rx.holdDyn (Left "") $ Right <$> eFormSubmitted
  -- eReqRes  <- echoForm dReqBody $ void eFormSubmitted
  let mkForm = \case
        Just f  -> Rx.text . TL.toStrict . Pretty.pShowNoColor $ f
        Nothing -> Rx.blank
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


mkValidation
  :: Rx.MonadWidget t m
  => Rx.Event t Submit
  -> m (Rx.InputElement Rx.EventResult (Rx.DomBuilderSpace m) t)
  -> (Text -> Validated a)
  -> m
       ( Rx.Event t (Map Rx.AttributeName (Maybe Text))
       , Rx.Dynamic t (Validated a)
       )
mkValidation eSubmit wInput validate = do
  input <- wInput
  let eDoneEditing = Rx.leftmost [void eSubmit, Rx.domEvent Rx.Blur input]
      bValue       = fmap validate . Rx.current . Rx._inputElement_value $ input
      dValue       = fmap validate . Rx._inputElement_value $ input
      eValue       = Rx.tag bValue eDoneEditing
  Rx.widgetHold_ Rx.blank . Rx.ffor eValue $ \case
    Failure es -> forM_ es $ Rx.elClass "p" "help is-danger" . Rx.text
    Success _  -> Rx.blank
  let eModifyAttrs = Rx.ffor eValue $ \case
        Failure _ -> [("class", Just "input is-danger")]
        Success _ -> [("class", Just "input is-success")]
  pure (eModifyAttrs, dValue)
