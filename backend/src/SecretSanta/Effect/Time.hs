{-# LANGUAGE UndecidableInstances #-}
module SecretSanta.Effect.Time
  ( GetTime
  , getTimeZone
  , getUTCTime
  , runGetTime
  )
where

import qualified "common" Data.Time            as Time
import qualified Data.Time.MonadTime           as Time

import           Polysemy

data GetTime m a where
  GetTimeZone ::GetTime m Time.TimeZone
  GetUTCTime ::GetTime m Time.UTCTime

makeSem ''GetTime

instance Member GetTime r => Time.MonadTime (Sem r) where
  getTimeZone = getTimeZone
  getUTCTime  = getUTCTime

runGetTime :: Member (Embed IO) r => Sem (GetTime ': r) a -> Sem r a
runGetTime = interpret $ \case
  GetTimeZone -> embed @IO Time.getTimeZone
  GetUTCTime  -> embed @IO Time.getUTCTime
