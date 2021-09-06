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

import qualified Data.Aeson                    as Aeson
import           Data.Refine
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

newtype TimeZone = TimeZone { unTimeZone :: Time.TimeZone }
  deriving newtype (Eq, Ord, Read, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via Refinable Text TimeZone

instance Refine Text TimeZone where
  unrefine = show . unTimeZone
  refine   = readMaybe . T.unpack |>? "Reading time zone failed"

getCurrentTimeZone :: IO TimeZone
getCurrentTimeZone = TimeZone <$> Time.getCurrentTimeZone

utcToZonedTime :: TimeZone -> UTCTime -> ZonedTime
utcToZonedTime (TimeZone timeZone) utcTime =
  Time.utcToZonedTime timeZone utcTime

newtype Date = Date { unDate :: Time.Day }
  deriving newtype (Show, Read, Eq, Aeson.ToJSON, Aeson.FromJSON)
validateDateMaybe :: Text -> Refined (Maybe Date)
validateDateMaybe = readValidationMaybe

instance Refine Text Date where
  unrefine = show
  refine   = readMaybe . T.unpack |>? "Reading date failed"

newtype Time = Time { unTime :: Time.TimeOfDay }
  deriving newtype (Eq, Show, Read)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via Refinable Text Time

validateTimeMaybe :: Text -> Maybe Time
validateTimeMaybe = \case
  (T.unpack -> [h1 , h2 , ':' , m1 , m2]) -> do
    h' <- readMaybe [h1, h2]
    m' <- readMaybe [m1, m2]
    Time <$> Time.makeTimeOfDayValid h' m' 0
  _ -> Nothing

instance Refine Text Time where
  refine = validateTimeMaybe |>? "Reading time failed"
  unrefine (Time (Time.TimeOfDay h m _s)) = show h <> ":" <> show m

validateDateTime
  :: Time.ZonedTime -> TimeZone -> Maybe Date -> Maybe Time -> Refined ()
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
            _  -> Failure "Must be in future"

compareZonedTime :: Time.ZonedTime -> Time.ZonedTime -> Ordering
compareZonedTime zt1 zt2 = comparing Time.zonedTimeToUTC zt1 zt2
