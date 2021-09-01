{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Polysemy.Transaction
  ( -- * Utils
    CanTransact(..)
  , Transaction(..)
  , transact
  , runTransaction
  ) where

import qualified Database.SQLite.Simple        as SQLite
import           Polysemy
import           Polysemy.Operators

-- * Connection class

class CanTransact c where
  startTransaction :: c -> IO ~@> ()
  endTransaction :: c -> IO ~@> ()
  rollbackTransaction :: c -> IO ~@> ()

instance CanTransact SQLite.Connection where
  startTransaction conn = embed $ SQLite.execute_ conn "BEGIN TRANSACTION"
  endTransaction conn = embed $ SQLite.execute_ conn "COMMIT TRANSACTION"
  rollbackTransaction conn =
    embed $ SQLite.execute_ conn "ROLLBACK TRANSACTION"

-- * Transactions

data Transaction c m a where
  Transact ::(c -> IO a) -> Transaction c m a
makeSem ''Transaction

runTransaction'
  :: Member (Embed IO) r => c -> Transaction c ': r @> a -> r @> a
runTransaction' conn = interpret $ \case
  Transact f -> embed $ f conn

runTransaction
  :: (Member (Embed IO) r, CanTransact c)
  => c
  -> Transaction c ': r @> Either e a
  -> r @> Either e a
runTransaction conn act = do
  startTransaction conn
  eRes <- runTransaction' conn act
  case eRes of
    Left  _ -> rollbackTransaction conn
    Right _ -> endTransaction conn
  pure eRes
