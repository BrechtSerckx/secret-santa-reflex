{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Time
  ( module Export
  , TimeZone(..)
  , getCurrentTimeZone
  , utcToZonedTime
  , Date(..)
  , validateDateMaybe
  , Time(..)
  , validateTimeMaybe
  , validateDateTime
  , compareZonedTime
  ) where

import           Control.Monad.Fail             ( fail )
import qualified Data.Aeson                    as Aeson
import qualified Data.Text                     as T
import "time"    Data.Time                     as Export
                                         hiding ( TimeZone(..)
                                                , getCurrentTimeZone
                                                , getTimeZone
                                                , getZonedTime
                                                , utcToZonedTime
                                                )
import qualified "time" Data.Time              as Time
import           Data.Validate
import qualified Text.Read                     as Read
import qualified Text.Show                     as Show

newtype TimeZone = TimeZone { unTimeZone :: Time.TimeZone }
  deriving newtype (Eq, Ord, Read)

instance Show TimeZone where
  show = Time.timeZoneOffsetString . unTimeZone

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
  toJSON = Aeson.String . T.pack . show

getCurrentTimeZone :: IO TimeZone
getCurrentTimeZone = TimeZone <$> Time.getCurrentTimeZone

utcToZonedTime :: TimeZone -> UTCTime -> ZonedTime
utcToZonedTime (TimeZone timeZone) utcTime =
  Time.utcToZonedTime timeZone utcTime

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

validateDateTime
  :: Time.ZonedTime -> TimeZone -> Maybe Date -> Maybe Time -> Validated ()
validateDateTime serverTime (TimeZone zonedTimeZone) mDate mTime =
  case mDate of
    Nothing -> pure ()
    Just (Date date) ->
      let clientTime = case mTime of
            Nothing ->
              let localDay             = Time.addDays 1 date
                  localTimeOfDay       = Time.midnight
                  zonedTimeToLocalTime = Time.LocalTime { .. }
              in  Time.ZonedTime { .. }
            Just (Time localTimeOfDay) ->
              let localDay             = date
                  zonedTimeToLocalTime = Time.LocalTime { .. }
              in  Time.ZonedTime { .. }
      in  case compareZonedTime serverTime clientTime of
            LT -> pure ()
            _  -> failure "Must be in future" -- TODO: decent errors

compareZonedTime :: Time.ZonedTime -> Time.ZonedTime -> Ordering
compareZonedTime zt1 zt2 = comparing Time.zonedTimeToUTC zt1 zt2
