{-# LANGUAGE RankNTypes #-}
module SecretSanta.Data
  ( SecretSantaId(..)
  , UnsafeSecretSanta(..)
  , SecretSanta(..)
  , validateSecretSanta
  , Info(..)
  , Participants
  , EventName
  , validateEventName
  , HostName
  , validateHostName
  , HostEmail
  , validateHostEmail
  , Location
  , validateLocationMaybe
  , Price
  , validatePriceMaybe
  , Description
  , validateDescription
  , Participant(..)
  , PName
  , validatePName
  , validatePNameUnique
  , PEmail
  , validatePEmail
  , validatePEmailUnique
  , Sender(..)
  ) where

import qualified Data.Aeson                    as Aeson
import qualified Data.List                     as L
import           Data.Refine
import qualified "this" Data.Time              as Time
import           Data.UUID
import           Data.Validate
import           Text.EmailAddress
import           Text.NonEmpty

newtype Sender = Sender EmailAddress

newtype SecretSantaId = SecretSantaId { secretsantaId :: UUID }
  deriving stock Generic
  deriving newtype (Eq, Ord, Show)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

-- * Secret Santa

-- | Base secret santa
data UnsafeSecretSanta = UnsafeSecretSanta
  { secretsantaInfo         :: Info
  , secretsantaParticipants :: Participants
  }
  deriving Generic
  deriving stock (Show, Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

-- | Secret santa with checked participants
newtype SecretSanta = SecretSanta { unSecretSanta :: UnsafeSecretSanta}
  deriving stock Generic
  deriving newtype (Show, Eq)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via Refinable UnsafeSecretSanta SecretSanta

instance Refine UnsafeSecretSanta SecretSanta where
  refine ss@UnsafeSecretSanta {..} = do
    not (unique $ pName <$> secretsantaParticipants)
      |> "Participant names must be unique."
    not (unique $ pEmail <$> secretsantaParticipants)
      |> "Participant emails must be unique."
    (length secretsantaParticipants < 3)
      |> "There must be at least 3 participants to ensure random matches."
    pure $ SecretSanta ss
    where unique l = length l == length (L.nub l)

validateSecretSanta :: UnsafeSecretSanta -> Refined SecretSanta
validateSecretSanta = refine

-- * Secret santa information

data Info = Info
  { iEventName   :: EventName
  , iHostName    :: HostName
  , iHostEmail   :: HostEmail
  , iTimeZone    :: Time.TimeZone
  , iDate        :: Maybe Time.Date
  , iTime        :: Maybe Time.Time
  , iLocation    :: Maybe Location
  , iPrice       :: Maybe Price
  , iDescription :: Description
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- * Secret santa participants

type Participants = [Participant]

-- ** Basic information

type EventName = NonEmptyText

validateEventName :: Text -> Refined EventName
validateEventName = refine

type HostName = NonEmptyText

validateHostName :: Text -> Refined HostName
validateHostName = refine

type HostEmail = EmailAddress

validateHostEmail :: Text -> Refined HostEmail
validateHostEmail = refine

type Location = NonEmptyText

validateLocationMaybe :: Text -> Refined (Maybe Location)
validateLocationMaybe = refineTextMaybe

newtype Price = Price Double
  deriving newtype (Show, Eq)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via Refinable Double Price

instance Refine Double Price where
  refine g = (g < 0) |> "Price can not be negative." $> Price g

validatePriceMaybe :: Text -> Refined (Maybe Price)
validatePriceMaybe = refineTextReadMaybe


type Description = NonEmptyText

validateDescription :: Text -> Refined Description
validateDescription = refine

-- ** Participants

data Participant = Participant
  { pName  :: PName
  , pEmail :: PEmail
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

type PName = NonEmptyText

validatePName :: Text -> Refined PName
validatePName = refine

validatePNameUnique :: PName -> [Refined PName] -> Maybe RefineErrors
validatePNameUnique name names =
  getFailure $ (name `elem` allSuccesses names) |> "Name must be unique"

type PEmail = EmailAddress

validatePEmail :: Text -> Refined PEmail
validatePEmail = refine


validatePEmailUnique :: PEmail -> [Refined PEmail] -> Maybe RefineErrors
validatePEmailUnique email emails =
  getFailure $ (email `elem` allSuccesses emails) |> "Email must be unique"
