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
  , Date
  , validateDateMaybe
  , Time
  , validateTimeMaybe
  , Location
  , validateLocationMaybe
  , Price
  , validatePriceMaybe
  , Description
  , validateDescription
  , Participant(..)
  , PName
  , validatePName
  , PEmail
  , validatePEmail
  ) where

import           Control.Monad.Fail             ( fail )
import qualified Data.Aeson                    as Aeson
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Time                     as Time
import           Text.EmailAddress
import           Text.NonEmpty
import qualified Text.Read                     as Read
import qualified Text.Show                     as Show

import           Data.Refine
import           Data.Validate

-- * Secret Santa form

data UnsafeForm = UnsafeForm
  { fEventName    :: EventName
  , fHostName     :: HostName
  , fHostEmail    :: HostEmail
  , fDate         :: Maybe Date
  , fTime         :: Maybe Time
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
  rguard f@UnsafeForm {..} = mconcat
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

newtype Date = Date { unDate :: Time.Day }
  deriving newtype (Show, Read, Eq, Aeson.ToJSON, Aeson.FromJSON)
validateDateMaybe :: Text -> Validated (Maybe Date)
validateDateMaybe = readValidationMaybe

newtype Time = Time { unTime :: Time.TimeOfDay }
  deriving newtype Eq

instance Show.Show Time where
  show (Time (Time.TimeOfDay h m _s)) = show h <> ":" <> show m

instance Read.Read Time where
  readsPrec _ = \case
    (h1 : h2 : ':' : m1 : m2 : rest) -> maybe [] pure $ do
      h' <- readMaybe [h1, h2]
      m' <- readMaybe [m1, m2]
      (, rest) . Time <$> Time.makeTimeOfDayValid h' m' 0
    _ -> []

instance Aeson.FromJSON Time where
  parseJSON = Aeson.withText "Time" $ \t -> case readMaybe . T.unpack $ t of
    Just r  -> pure r
    Nothing -> invalidTime
    where invalidTime = fail "Invalid time. Format: hh:mm"

instance Aeson.ToJSON Time where
  toJSON = Aeson.String . show

validateTimeMaybe :: Text -> Validated (Maybe Time)
validateTimeMaybe = readValidationMaybe

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

validatePNameUnique :: PName -> [Validated PName] -> Validated PName
validatePNameUnique name names =
  if (length . filter (== Success name) $ names) == 1
    then pure name
    else Failure . pure $ "Name must be unique"

type PEmail = EmailAddress

validatePEmail :: Text -> Validated PEmail
validatePEmail = validateEmailAddress
