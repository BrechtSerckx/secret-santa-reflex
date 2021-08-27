{-# LANGUAGE AllowAmbiguousTypes #-}
module SecretSanta.Backend.KVStore.Database.Sqlite
  ( Sqlite
  ) where

import qualified Database.Beam.Sqlite.Connection
                                               as Beam
import qualified Database.SQLite.Simple        as SQLite
import           Polysemy.Transaction.Beam
import           SecretSanta.Backend.KVStore.Database.Class
import qualified Options.Applicative           as OA
import Polysemy.Input

data Sqlite

instance IsDatabaseBackend Sqlite where
  type BeamBackend Sqlite = Beam.Sqlite
  type BeamBackendM Sqlite = Beam.SqliteM
  type DBConnection Sqlite = SQLite.Connection
  type DBConfig Sqlite = FilePath
  type DBOpts Sqlite = FilePath
  runBeamTransaction = runBeamTransactionSqlite
  withDBConnection db f = SQLite.withConnection db $ \conn -> do
    SQLite.setTrace conn $ Just putStrLn
    f conn
  runDBConfig conn = runInputConst conn
  parseDBOpts =
    OA.strOption $ mconcat [OA.long "sqlite", OA.metavar "SQLITE_DATABASE"]
