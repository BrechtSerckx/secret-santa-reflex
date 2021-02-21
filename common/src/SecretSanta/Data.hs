{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module SecretSanta.Data where

import           Control.Monad.Fail             ( fail )
import qualified Data.Aeson                    as Aeson
import           Data.Either.Validation
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Time                     as Time
import           Text.EmailAddress              ( EmailAddress
                                                , emailAddressFromText
                                                )

import           Data.Typeable                  ( typeOf )

import           Data.Coerce                    ( Coercible
                                                , coerce
                                                )
import qualified Text.Read                     as Read
import qualified Text.Show                     as Show

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
  deriving newtype (Eq, Show, Read)

instance Aeson.FromJSON Time where
  parseJSON = Aeson.withText "Time" $ \t -> case T.splitOn ":" t of
    [h, m] ->
      let mTime = do
            h' <- readMaybe $ T.unpack h
            m' <- readMaybe $ T.unpack m
            Time <$> Time.makeTimeOfDayValid h' m' 0
      in  case mTime of
            Just r  -> pure r
            Nothing -> invalidTime
    _ -> invalidTime
    where invalidTime = fail "Invalid time. Format: hh:mm"

instance Aeson.ToJSON Time where
  toJSON (Time (Time.TimeOfDay h m _s)) =
    Aeson.String $ show h <> ":" <> show m

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

-- * Utils

validateEmailAddress :: Text -> Validated EmailAddress
validateEmailAddress t
  | T.null t  = Failure . pure $ "Email cannot be empty"
  | otherwise = maybe invalidEmail Success . emailAddressFromText $ t
 where
  invalidEmail =
    Failure . pure $ "Invalid email. Format: my-email-adress@my-provider"

type Validated a = Validation [Text] a


readValidation :: Read a => Text -> Validated a
readValidation t = case readMaybe . T.unpack $ t of
  Nothing -> Failure . pure $ "Cannot read value."
  Just a  -> pure a

readValidationMaybe :: Read a => Text -> Validated (Maybe a)
readValidationMaybe t
  | T.null t = pure Nothing
  | otherwise = fmap Just $ case readMaybe . T.unpack $ t of
    Nothing -> Failure . pure $ "Cannot read value."
    Just a  -> pure a


newtype Refined from to = UnsafeRefined { unrefine :: to }
  deriving newtype (Eq, Show, Aeson.ToJSON)

instance (Aeson.FromJSON from, Refine from to) => Aeson.FromJSON (Refined from to) where
  parseJSON v = Aeson.parseJSON v >>= \a -> case refine @from @to a of
    Failure es -> fail . show $ es
    Success a' -> pure . UnsafeRefined $ a'

class Refine from to | to -> from where

  rguard :: from -> [Text]

  construct :: from -> to
  default construct :: Coercible from to => from -> to
  construct = coerce @from @to

refine :: forall from to . Refine from to => from -> Validated to
refine fa = case rguard @from @to fa of
  [] -> Success . construct $ fa
  es -> Failure es

refineTextMaybe :: Refine Text to => Text -> Validated (Maybe to)
refineTextMaybe t | T.null t  = Success Nothing
                  | otherwise = Just <$> refine t

refineTextReadMaybe
  :: (Read from, Refine from to) => Text -> Validated (Maybe to)
refineTextReadMaybe t
  | T.null t  = Success Nothing
  | otherwise = fmap Just $ readValidation t `bindValidation` refine

bindValidation :: Validated a -> (a -> Validated b) -> Validated b
bindValidation a f = case a of
  Failure es -> Failure es
  Success a  -> f a

(|>) :: Bool -> Text -> [Text]
cond |> err = if cond then [err] else []
infixl 0 |>


newtype NonEmptyText = NonEmptyText { unNonEmptyText :: Text}
  deriving newtype (Show, Eq)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Refined Text NonEmptyText)

instance Refine Text NonEmptyText where
  rguard text = T.null text |> "Cannot be empty."
