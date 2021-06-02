{-# LANGUAGE RankNTypes #-}
module SecretSanta.Data
  ( UnsafeSecretSantaT(..)
  , UnsafeSecretSanta
  , SecretSantaT(..)
  , SecretSanta
  , validateSecretSanta
  , IntT(..)
  , InfoT(..)
  , Info
  , ParticipantsT
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
  , ParticipantT(..)
  , Participant
  , PName
  , validatePName
  , validatePNameUnique
  , PEmail
  , validatePEmail
  , validatePEmailUnique
  , Sender(..)
  , WithPrimaryKeyT(..)
  , WithPrimaryKey
  ) where

import qualified Data.Aeson                    as Aeson
import qualified Data.List                     as L
import qualified "this" Data.Time              as Time
import           Text.EmailAddress
import           Text.NonEmpty

import           Data.Refine
import           Data.Validate

import           Database.Beam

newtype Sender = Sender EmailAddress

data WithPrimaryKeyT k t (f :: * -> *) = WithPrimaryKey
  { key   :: k f
  , value :: t f
  }
  deriving stock Generic
  deriving anyclass Beamable
type WithPrimaryKey k t = WithPrimaryKeyT k t Identity

-- * Secret Santa

-- | Base secret santa
data UnsafeSecretSantaT f = UnsafeSecretSanta
  { secretsantaInfo         :: InfoT f
  , secretsantaParticipants :: ParticipantsT f
  }
  deriving Generic
  deriving anyclass Beamable
type UnsafeSecretSanta = UnsafeSecretSantaT Identity

deriving stock instance Show UnsafeSecretSanta
deriving stock instance Eq UnsafeSecretSanta
deriving anyclass instance Aeson.ToJSON UnsafeSecretSanta
deriving anyclass instance Aeson.FromJSON UnsafeSecretSanta

-- | Secret santa with checked participants
newtype SecretSantaT f = SecretSanta { unSecretSanta :: UnsafeSecretSantaT f}
  deriving Generic
  deriving anyclass Beamable
type SecretSanta = SecretSantaT Identity

deriving newtype instance Show SecretSanta
deriving newtype instance Eq SecretSanta
deriving via Refined UnsafeSecretSanta SecretSanta instance Aeson.ToJSON SecretSanta
deriving via Refined UnsafeSecretSanta SecretSanta instance Aeson.FromJSON SecretSanta

instance Refine UnsafeSecretSanta SecretSanta where
  rguard UnsafeSecretSanta {..} = mconcat
    [ not (unique $ pName <$> secretsantaParticipants)
      |> "Participant names must be unique."
    , not (unique $ pEmail <$> secretsantaParticipants)
      |> "Participant emails must be unique."
    , length secretsantaParticipants
    <  3
    |> "There must be at least 3 participants to ensure random matches."
    ]
    where unique l = length l == length (L.nub l)

validateSecretSanta :: UnsafeSecretSanta -> Validated SecretSanta
validateSecretSanta = refine

-- * Secret santa information

data InfoT f = Info
  { iEventName   :: Columnar f EventName
  , iHostName    :: Columnar f HostName
  , iHostEmail   :: Columnar f HostEmail
  , iTimeZone    :: Columnar f Time.TimeZone
  , iDate        :: Columnar f (Maybe Time.Date)
  , iTime        :: Columnar f (Maybe Time.Time)
  , iLocation    :: Columnar f (Maybe Location)
  , iPrice       :: Columnar f (Maybe Price)
  , iDescription :: Columnar f Description
  }
  deriving stock Generic
  deriving anyclass Beamable
type Info = InfoT Identity

deriving stock instance Show Info
deriving stock instance Eq Info
deriving anyclass instance Aeson.FromJSON Info
deriving anyclass instance Aeson.ToJSON Info


newtype IntT f = IntT (Columnar f Int)
  deriving stock Generic
  deriving anyclass Beamable
instance Table (WithPrimaryKeyT IntT InfoT) where
  data PrimaryKey (WithPrimaryKeyT IntT InfoT) f = InfoId (IntT f)
    deriving (Generic, Beamable)
  primaryKey (WithPrimaryKey key _val) = InfoId key

-- * Secret santa participants

type ParticipantsT f = Columnar f [Participant]
type Participants = ParticipantsT Identity



-- ** Basic information

type EventName = NonEmptyText

validateEventName :: Text -> Validated EventName
validateEventName = refine

type HostName = NonEmptyText

validateHostName :: Text -> Validated HostName
validateHostName = refine

type HostEmail = EmailAddress

validateHostEmail :: Text -> Validated HostEmail
validateHostEmail = refine

type Location = NonEmptyText

validateLocationMaybe :: Text -> Validated (Maybe Location)
validateLocationMaybe = refineTextMaybe

newtype Price = Price Double
  deriving newtype (Show, Eq)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via Refined Double Price

instance Refine Double Price where
  rguard g = g < 0 |> "Price can not be negative."

validatePriceMaybe :: Text -> Validated (Maybe Price)
validatePriceMaybe = refineTextReadMaybe


type Description = NonEmptyText

validateDescription :: Text -> Validated Description
validateDescription = refine

-- ** Participants

data ParticipantT f = Participant
  { pName  :: Columnar f PName
  , pEmail :: Columnar f PEmail
  }
  deriving stock Generic
  deriving anyclass Beamable

type Participant = ParticipantT Identity
deriving stock instance Show Participant
deriving stock instance Eq Participant
deriving anyclass instance Aeson.FromJSON Participant
deriving anyclass instance Aeson.ToJSON Participant

instance Table (WithPrimaryKeyT IntT ParticipantT) where
  data PrimaryKey (WithPrimaryKeyT IntT ParticipantT) f = ParticipantId
    { piSecretSanta :: IntT f
    , piPName :: Columnar f PName
  
    }
    deriving (Generic, Beamable)
  primaryKey (WithPrimaryKey key p) = ParticipantId key $ pName p

type PName = NonEmptyText

validatePName :: Text -> Validated PName
validatePName = refine

validatePNameUnique :: PName -> [Validated PName] -> [Text]
validatePNameUnique name names = if notElem name $ allSuccesses names
  then mempty
  else pure "Name must be unique"

type PEmail = EmailAddress

validatePEmail :: Text -> Validated PEmail
validatePEmail = refine


validatePEmailUnique :: PEmail -> [Validated PEmail] -> [Text]
validatePEmailUnique email emails = if notElem email $ allSuccesses emails
  then mempty
  else pure "Email must be unique"
