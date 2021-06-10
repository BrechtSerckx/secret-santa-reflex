{-# LANGUAGE RankNTypes #-}
module SecretSanta.Data
  ( SecretSantaIdT(..)
  , SecretSantaId
  , UnsafeSecretSantaT(..)
  , UnsafeSecretSanta
  , SecretSantaT(..)
  , SecretSanta
  , validateSecretSanta
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
  )
where

import qualified Data.Aeson                    as Aeson
import qualified Data.List                     as L
import           Data.Refine
import qualified "this" Data.Time              as Time
import           Data.UUID
import           Data.Validate
import           Database.Beam
import           Text.EmailAddress
import           Text.NonEmpty

newtype Sender = Sender EmailAddress

newtype SecretSantaIdT f = SecretSantaId { secretsantaId :: C f UUID }
  deriving stock Generic
type SecretSantaId = SecretSantaIdT Identity
deriving newtype instance Eq SecretSantaId
deriving newtype instance Ord SecretSantaId
deriving newtype instance Show SecretSantaId
deriving newtype instance Aeson.ToJSON SecretSantaId
deriving newtype instance Aeson.FromJSON SecretSantaId

-- * Secret Santa

-- | Base secret santa
data UnsafeSecretSantaT f = UnsafeSecretSanta
  { secretsantaInfo         :: InfoT f
  , secretsantaParticipants :: ParticipantsT f
  }
  deriving Generic
type UnsafeSecretSanta = UnsafeSecretSantaT Identity

deriving stock instance Show UnsafeSecretSanta
deriving stock instance Eq UnsafeSecretSanta
deriving anyclass instance Aeson.ToJSON UnsafeSecretSanta
deriving anyclass instance Aeson.FromJSON UnsafeSecretSanta

-- | Secret santa with checked participants
newtype SecretSantaT f = SecretSanta { unSecretSanta :: UnsafeSecretSantaT f}
  deriving Generic
type SecretSanta = SecretSantaT Identity

deriving newtype instance Show SecretSanta
deriving newtype instance Eq SecretSanta
deriving via Refined UnsafeSecretSanta SecretSanta instance Aeson.ToJSON SecretSanta
deriving via Refined UnsafeSecretSanta SecretSanta instance Aeson.FromJSON SecretSanta

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

validateSecretSanta :: UnsafeSecretSanta -> Validated SecretSanta
validateSecretSanta = refine

-- * Secret santa information

data InfoT f = Info
  { iEventName   :: C f EventName
  , iHostName    :: C f HostName
  , iHostEmail   :: C f HostEmail
  , iTimeZone    :: C f Time.TimeZone
  , iDate        :: C f (Maybe Time.Date)
  , iTime        :: C f (Maybe Time.Time)
  , iLocation    :: C f (Maybe Location)
  , iPrice       :: C f (Maybe Price)
  , iDescription :: C f Description
  }
  deriving stock Generic
type Info = InfoT Identity

deriving stock instance Show Info
deriving stock instance Eq Info
deriving anyclass instance Aeson.FromJSON Info
deriving anyclass instance Aeson.ToJSON Info

-- * Secret santa participants

type ParticipantsT f = C f [Participant]
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
  refine g = (g < 0) |> "Price can not be negative." $> Price g

validatePriceMaybe :: Text -> Validated (Maybe Price)
validatePriceMaybe = refineTextReadMaybe


type Description = NonEmptyText

validateDescription :: Text -> Validated Description
validateDescription = refine

-- ** Participants

data ParticipantT f = Participant
  { pName  :: C f PName
  , pEmail :: C f PEmail
  }
  deriving stock Generic

type Participant = ParticipantT Identity
deriving stock instance Show Participant
deriving stock instance Eq Participant
deriving anyclass instance Aeson.FromJSON Participant
deriving anyclass instance Aeson.ToJSON Participant

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
