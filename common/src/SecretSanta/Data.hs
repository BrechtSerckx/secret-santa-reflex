{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SecretSanta.Data where

import           Control.Monad.Fail             ( fail )
import qualified Data.Aeson                    as Aeson
import           Data.Either.Validation
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Time                      ( Day
                                                , TimeOfDay
                                                , makeTimeOfDayValid
                                                )
import           Text.EmailAddress

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

newtype Name = Name Text
  deriving stock Generic
  deriving newtype (Show, Read)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

validateName :: Text -> Validated Name
validateName text | T.null text = Failure . pure $ "Name cannot be empty"
                  | otherwise   = Success . Name $ text

-- ** Event date

validateDate :: Text -> Validated (Maybe Day)
validateDate t
  | T.null t
  = pure Nothing
  | otherwise
  = fmap Just
    . eitherToValidation
    . first (pure . T.pack)
    . readEither
    . T.unpack
    $ t

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

newtype Location = Location Text
  deriving stock Generic
  deriving newtype (Show, Read)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

validateLocation :: Text -> Validated (Maybe Location)
validateLocation = Success . Just . Location

-- ** Gift price

newtype Price = Price Double
  deriving stock Generic
  deriving newtype (Show, Read)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

validatePrice :: Text -> Validated (Maybe Price)
validatePrice t
  | T.null t = pure Nothing
  | otherwise = case readMaybe @Price . T.unpack $ t of
    Nothing -> Failure . pure $ "Price must be a valid decimal number."
    Just d  -> pure . Just $ d

-- ** Event description

newtype Description = Description Text
  deriving stock Generic
  deriving newtype (Show, Read)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

validateDescription :: Text -> Validated Description
validateDescription t
  | T.null t  = Failure . pure $ "Description cannot be empty"
  | otherwise = pure . Description $ t

-- ** Participants

data Participant = Participant
  { pName  :: PName
  , pEmail :: PEmail
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

-- *** Participant name

newtype PName = PName Text
  deriving stock Generic
  deriving newtype (Show, Read, Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

validatePName :: Text -> Validated PName
validatePName t | T.null t  = Failure . pure $ "Name cannot be empty"
                | otherwise = Success . PName $ t

validatePNameUnique :: PName -> [Validated PName] -> Validated PName
validatePNameUnique name names =
  if (length . filter (== Success name) $ names) == 1
    then pure name
    else Failure . pure $ "Name must be unique"

-- *** Participant Email

newtype PEmail = PEmail EmailAddress
  deriving stock Generic
  deriving newtype (Show, Read, Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

validatePEmail :: Text -> Validated PEmail
validatePEmail t
  | T.null t  = Failure . pure $ "Email cannot be empty"
  | otherwise = maybe invalidEmail (Success . PEmail) . emailAddressFromText $ t
 where
  invalidEmail =
    Failure . pure $ "Invalid email. Format: my-email-adress@my-provider"

-- * Utils

type Validated a = Validation [Text] a
