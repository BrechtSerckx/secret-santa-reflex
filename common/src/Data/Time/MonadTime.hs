module Data.Time.MonadTime
  ( MonadTime(..)
  , getZonedTime
  )
where

import "this"    Data.Time

class Monad m => MonadTime m where
  getTimeZone :: m TimeZone
  getUTCTime :: m UTCTime

instance MonadTime IO where
  getTimeZone = getCurrentTimeZone
  getUTCTime  = getCurrentTime

getZonedTime :: MonadTime m => m ZonedTime
getZonedTime = do
  timeZone <- getTimeZone
  utcTime  <- getUTCTime
  pure $ utcToZonedTime timeZone utcTime
