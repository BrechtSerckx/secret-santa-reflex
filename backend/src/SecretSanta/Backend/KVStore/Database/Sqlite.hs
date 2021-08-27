{-# LANGUAGE AllowAmbiguousTypes #-}
module SecretSanta.Backend.KVStore.Database.Sqlite
  ( Sqlite
  ) where

import qualified Database.Beam.Sqlite.Connection
                                               as Beam
import qualified Database.SQLite.Simple        as SQLite
import           Polysemy.Transaction.Beam
import           SecretSanta.Backend.KVStore.Database.Class

data Sqlite

instance IsDatabaseBackend Sqlite where
  type DBConnection Sqlite = SQLite.Connection
  type BeamBackend Sqlite = Beam.Sqlite
  type BeamBackendM Sqlite = Beam.SqliteM
  runBeamTransaction = runBeamTransactionSqlite
