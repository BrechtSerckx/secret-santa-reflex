{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Polysemy.Transaction
  ( -- * Utils
    Connection(..)
  , Transaction(..)
  , transact
  , runTransaction
  )
where

import qualified Database.SQLite.Simple        as SQLite
import           Polysemy
import           Polysemy.Operators
import qualified Options.Applicative           as OA

-- * Connection class

class Connection c where
  startTransaction :: c -> IO ~@> ()
  endTransaction :: c -> IO ~@> ()
  rollbackTransaction :: c -> IO ~@> ()
  type WithConnectionInput c :: Type
  withConnection :: WithConnectionInput c -> (c -> IO a) -> IO a
  parseConn :: OA.Parser (WithConnectionInput c)

instance Connection SQLite.Connection where
  startTransaction conn = embed $ SQLite.execute_ conn "BEGIN TRANSACTION"
  endTransaction conn = embed $ SQLite.execute_ conn "COMMIT TRANSACTION"
  rollbackTransaction conn =
    embed $ SQLite.execute_ conn "ROLLBACK TRANSACTION"
  type WithConnectionInput SQLite.Connection = FilePath
  withConnection db f = SQLite.withConnection db $ \conn -> do
    SQLite.setTrace conn $ Just putStrLn
    f conn
  parseConn =
    OA.strOption $ mconcat [OA.long "sqlite", OA.metavar "SQLITE_DATABASE"]

-- * Transactions

data Transaction c m a where
  Transact ::(c -> IO a) -> Transaction c m a
makeSem ''Transaction

runTransaction' :: Member (Embed IO) r => c -> Transaction c ': r @> a -> r @> a
runTransaction' conn = interpret $ \case
  Transact f -> embed $ f conn

runTransaction
  :: (Member (Embed IO) r, Connection c)
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
