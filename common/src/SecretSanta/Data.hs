module SecretSanta.Data
  ( Form(..)
  , validateForm
  , UnsafeForm(..)
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
  )
where

import qualified Data.Aeson                    as Aeson
import qualified Data.List                     as L
import qualified "this" Data.Time              as Time
import           Text.EmailAddress
import           Text.NonEmpty

import           Data.Refine
import           Data.Validate

newtype Sender = Sender EmailAddress

-- * Secret Santa form

data UnsafeForm = UnsafeForm
  { fEventName    :: EventName
  , fHostName     :: HostName
  , fHostEmail    :: HostEmail
  , fTimeZone     :: Time.TimeZone
  , fDate         :: Maybe Time.Date
  , fTime         :: Maybe Time.Time
  , fLocation     :: Maybe Location
  , fPrice        :: Maybe Price
  , fDescription  :: Description
  , fParticipants :: [Participant]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

newtype Form = Form UnsafeForm
  deriving newtype (Show, Eq)
  deriving  (Aeson.ToJSON, Aeson.FromJSON) via (Refined UnsafeForm Form)

instance Refine UnsafeForm Form where
  rguard UnsafeForm {..} = mconcat
    [ not (unique $ pName <$> fParticipants)
      |> "Participant names must be unique."
    , not (unique $ pEmail <$> fParticipants)
      |> "Participant emails must be unique."
    , length fParticipants
    <  3
    |> "There must be at least 3 participants to ensure random matches."
    ]
    where unique l = length l == length (L.nub l)

validateForm :: UnsafeForm -> Validated Form
validateForm = refine

-- ** Basic information

type EventName = NonEmptyText

validateEventName :: Text -> Validated EventName
validateEventName = refine

type HostName = NonEmptyText

validateHostName :: Text -> Validated HostName
validateHostName = refine

type HostEmail = EmailAddress

validateHostEmail :: Text -> Validated HostEmail
validateHostEmail = validateEmailAddress

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

data Participant = Participant
  { pName  :: PName
  , pEmail :: PEmail
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

type PName = NonEmptyText

validatePName :: Text -> Validated PName
validatePName = refine

validatePNameUnique :: PName -> [Validated PName] -> [Text]
validatePNameUnique name names = if notElem name $ allSuccesses names
  then mempty
  else pure "Name must be unique"

type PEmail = EmailAddress

validatePEmail :: Text -> Validated PEmail
validatePEmail = validateEmailAddress


validatePEmailUnique :: PEmail -> [Validated PEmail] -> [Text]
validatePEmailUnique email emails = if notElem email $ allSuccesses emails
  then mempty
  else pure "Email must be unique"
