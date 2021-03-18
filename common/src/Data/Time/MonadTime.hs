module Data.Time.MonadTime
  ( MonadTime(..)
  , getZonedTime
  , compareZonedTime
  , module Export
  ) where

import           Data.Time                     as Export
                                         hiding ( getCurrentTime
                                                , getCurrentTimeZone
                                                , getTimeZone
                                                , getZonedTime
                                                , utcToLocalZonedTime
                                                )
import qualified Data.Time                     as Time

class Monad m => MonadTime m where
  getTimeZone :: m TimeZone
  getUTCTime :: m UTCTime

instance MonadTime IO where
  getTimeZone = Time.getCurrentTimeZone
  getUTCTime  = Time.getCurrentTime

getZonedTime :: MonadTime m => m ZonedTime
getZonedTime = do
  timeZone <- getTimeZone
  utcTime  <- getUTCTime
  pure $ utcToZonedTime timeZone utcTime

compareZonedTime :: ZonedTime -> ZonedTime -> Ordering
compareZonedTime zt1 zt2 = comparing zonedTimeToUTC zt1 zt2
