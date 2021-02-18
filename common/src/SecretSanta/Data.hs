{-# LANGUAGE DeriveAnyClass #-}
module SecretSanta.Data where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Either.Validation
import qualified Data.Text                     as T
import           Data.Time                      ( Day
                                                , TimeOfDay
                                                , makeTimeOfDayValid
                                                )

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
  deriving anyclass (ToJSON, FromJSON)

-- ** Event Name

newtype Name = Name Text
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

validateLocation :: Text -> Validated (Maybe Location)
validateLocation = Success . Just . Location

-- ** Gift price

newtype Price = Price Double
  deriving stock (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

validatePrice :: Text -> Validated (Maybe Price)
validatePrice t
  | T.null t = pure Nothing
  | otherwise = case readMaybe . T.unpack $ t of
    Nothing -> Failure . pure $ "Price must be a valid decimal number."
    Just d  -> pure . Just $ d

-- ** Event description

newtype Description = Description Text
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  deriving anyclass (ToJSON, FromJSON)

-- *** Participant name

newtype PName = PName Text
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

validatePName :: Text -> Validated PName
validatePName t | T.null t  = Failure . pure $ "Name cannot be empty"
                | otherwise = Success . PName $ t

-- *** Participant Email

newtype PEmail = PEmail Text
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

validatePEmail :: Text -> Validated PEmail
validatePEmail t | T.null t  = Failure . pure $ "Email cannot be empty"
                 | otherwise = Success . PEmail $ t

-- * Utils

type Validated a = Validation [Text] a
