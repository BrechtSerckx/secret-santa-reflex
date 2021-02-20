{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SecretSanta.Data where

import qualified Data.Aeson                    as Aeson
import           Data.Either.Validation
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Time                      ( Day
                                                , TimeOfDay
                                                , makeTimeOfDayValid
                                                )
import           Text.EmailAddress              ( EmailAddress
                                                , emailAddressFromText
                                                )

import           Data.Typeable                  ( typeOf )
import           Refined
import           Refined.Orphan

-- * Secret Santa form

data Form = Form
  { fName         :: Name
  , fDate         :: Maybe Day
  , fTime         :: Maybe TimeOfDay
  , fLocation     :: Maybe Location
  , fPrice        :: Maybe Price
  , fDescription  :: Description
  , fParticipants :: [Participant]
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

validateForm :: Form -> Validated Form
validateForm f@Form {..} = do
  if unique $ pName <$> fParticipants
    then pure ()
    else Failure . pure $ "Participant names must be unique."
  if unique $ pEmail <$> fParticipants
    then pure ()
    else Failure . pure $ "Participant emails must be unique."
  if length fParticipants >= 3
    then pure ()
    else
      Failure
      . pure
      $ "There must be at least 3 participants to ensure random matches."
  pure f
  where unique l = length l == length (L.nub l)


-- ** Event Name

type Name = Refined NotEmpty Text
data NotEmpty
instance Predicate NotEmpty Text where
  validate p text =
    when (T.null text) . throwRefineOtherException (typeOf p) $ pretty @Text
      "name cannot be empty"
validateName :: Text -> Validated Name
validateName = validateRefined

-- ** Event date

validateDate :: Text -> Validated (Maybe Day)
validateDate t | T.null t  = pure Nothing
               | otherwise = Just <$> readValidation "Invalid date." t

-- ** Event time

validateTime :: Text -> Validated (Maybe TimeOfDay)
validateTime t
  | T.null t = pure Nothing
  | otherwise = case T.splitOn ":" t of
    [h, m] ->
      let mTime = do
            h' <- readMaybe $ T.unpack h
            m' <- readMaybe $ T.unpack m
            makeTimeOfDayValid h' m' 0
      in  case mTime of
            Just r  -> pure $ Just r
            Nothing -> invalidTime
    _ -> invalidTime
  where invalidTime = Failure . pure $ "Invalid time. Format: hh:mm"

-- ** Event location

type Location = Refined NotEmpty Text

validateLocation :: Text -> Validated (Maybe Location)
validateLocation t | T.null t  = pure Nothing
                   | otherwise = Just <$> validateRefined t

-- ** Gift price

newtype Price = Price Double
  deriving stock Generic
  deriving newtype (Show, Read)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

validatePrice :: Text -> Validated (Maybe Price)
validatePrice t
  | T.null t
  = pure Nothing
  | otherwise
  = Just <$> readValidation "Price must be a valid decimal number." t

-- ** Event description

type Description = Refined NotEmpty Text

validateDescription :: Text -> Validated Description
validateDescription = validateRefined

-- ** Participants

data Participant = Participant
  { pName  :: PName
  , pEmail :: PEmail
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

-- *** Participant name

type PName = Refined NotEmpty Text

validatePName :: Text -> Validated PName
validatePName = validateRefined

validatePNameUnique :: PName -> [Validated PName] -> Validated PName
validatePNameUnique name names =
  if (length . filter (== Success name) $ names) == 1
    then pure name
    else Failure . pure $ "Name must be unique"

-- *** Participant Email

type PEmail = EmailAddress

validatePEmail :: Text -> Validated PEmail
validatePEmail t
  | T.null t  = Failure . pure $ "Email cannot be empty"
  | otherwise = maybe invalidEmail Success . emailAddressFromText $ t
 where
  invalidEmail =
    Failure . pure $ "Invalid email. Format: my-email-adress@my-provider"

-- * Utils

type Validated a = Validation [Text] a


validateRefined :: Predicate p input => input -> Validated (Refined p input)
validateRefined =
  eitherToValidation . first (pure . T.pack . displayException) . refine

readValidation :: Read a => Text -> Text -> Validated a
readValidation errMsg t = case readMaybe . T.unpack $ t of
  Nothing -> Failure . pure $ errMsg
  Just a  -> pure a
