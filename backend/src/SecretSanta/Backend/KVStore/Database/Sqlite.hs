{-# LANGUAGE AllowAmbiguousTypes #-}
module SecretSanta.Backend.KVStore.Database.Sqlite
  ( Sqlite
  ) where

import qualified Data.Text                     as T
import           Database.Beam.Migrate
import qualified Database.Beam.Sqlite          as SQLite
import qualified Database.Beam.Sqlite.Connection
                                               as Beam
import           Database.Beam.Sqlite.Migrate   ( migrationBackend )
import qualified Database.SQLite.Simple        as SQLite
import qualified Options.Applicative           as OA
import           SecretSanta.Backend.KVStore.Database.Class

data SqliteOpts = SqliteOpts
  { sqliteDatabase  :: FilePath
  , sqliteTraceConn :: Bool
  }

data Sqlite

instance IsDatabaseBackend Sqlite where

  type BeamBackend Sqlite = Beam.Sqlite
  type BeamBackendM Sqlite = Beam.SqliteM
  runBeam              = SQLite.runBeamSqlite
  beamMigrationBackend = migrationBackend

  dbSettings           = defaultMigratableDbSettings

  type DBOpts Sqlite = SqliteOpts
  parseDBOpts = do
    sqliteDatabase <- OA.strOption
      $ mconcat [OA.long "sqlite", OA.metavar "SQLITE_DATABASE"]
    sqliteTraceConn <- OA.switch $ mconcat [OA.long "trace"]
    pure SqliteOpts { .. }

  type DBConnection Sqlite = SQLite.Connection
  createDBConnection SqliteOpts {..} = do
    conn <- SQLite.open sqliteDatabase
    when sqliteTraceConn . SQLite.setTrace conn . Just $ putStrLn . T.unpack
    pure conn
  closeDBConnection = SQLite.close
  withDBConnection SqliteOpts {..} f =
    SQLite.withConnection sqliteDatabase $ \conn -> do
      when sqliteTraceConn . SQLite.setTrace conn . Just $ putStrLn . T.unpack
      f conn
