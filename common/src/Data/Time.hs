{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Time
  ( module Export
  , Date(..)
  , validateDateMaybe
  , Time(..)
  , validateTimeMaybe
  , validateDateTime
  , compareZonedTime
  )
where

import           Control.Monad.Fail             ( fail )
import qualified Data.Aeson                    as Aeson
import "time"    Data.Time                     as Export
                                         hiding ( getTimeZone
                                                , getZonedTime
                                                )
import qualified Data.Text                     as T
import qualified Text.Read                     as Read
import qualified Text.Show                     as Show
import           Data.Validate

instance Aeson.FromJSON TimeZone where
  parseJSON = Aeson.withText "TimeZone" $ \t ->
    case readMaybe . T.unpack $ t of
      Just r  -> pure r
      Nothing -> invalidTimeZone
   where
    invalidTimeZone =
      fail
        "Invalid timezone. Allowed: ±HHMM format, single-letter military time-zones, and these time-zones: 'UTC', 'UT', 'GMT', 'EST', 'EDT', 'CST', 'CDT', 'MST', 'MDT', 'PST', 'PDT'."

instance Aeson.ToJSON TimeZone where
  toJSON = Aeson.String . T.pack . timeZoneOffsetString

newtype Date = Date { unDate :: Day }
  deriving newtype (Show, Read, Eq, Aeson.ToJSON, Aeson.FromJSON)
validateDateMaybe :: Text -> Validated (Maybe Date)
validateDateMaybe = readValidationMaybe

newtype Time = Time { unTime :: TimeOfDay }
  deriving newtype Eq

instance Show.Show Time where
  show (Time (TimeOfDay h m _s)) = show h <> ":" <> show m

instance Read.Read Time where
  readsPrec _ = \case
    (h1 : h2 : ':' : m1 : m2 : rest) -> maybe [] pure $ do
      h' <- readMaybe [h1, h2]
      m' <- readMaybe [m1, m2]
      (, rest) . Time <$> makeTimeOfDayValid h' m' 0
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

validateDateTime
  :: ZonedTime -> TimeZone -> Maybe Date -> Maybe Time -> Validated ()
validateDateTime serverTime zonedTimeZone mDate mTime = case mDate of
  Nothing -> pure ()
  Just (Date date) ->
    let clientTime = case mTime of
          Nothing ->
            let localDay             = addDays 1 date
                localTimeOfDay       = midnight
                zonedTimeToLocalTime = LocalTime { .. }
            in  ZonedTime { .. }
          Just (Time localTimeOfDay) ->
            let localDay             = date
                zonedTimeToLocalTime = LocalTime { .. }
            in  ZonedTime { .. }
    in  case compareZonedTime serverTime clientTime of
          LT -> pure ()
          _  -> failure "Must be in future" -- TODO: decent errors

compareZonedTime :: ZonedTime -> ZonedTime -> Ordering
compareZonedTime zt1 zt2 = comparing zonedTimeToUTC zt1 zt2
