{-# LANGUAGE AllowAmbiguousTypes #-}
module SecretSanta.Backend.KVStore.Database.Sqlite
  ( Sqlite
  ) where

import           Database.Beam.Migrate
import qualified Database.Beam.Sqlite          as SQLite
import qualified Database.Beam.Sqlite.Connection
                                               as Beam
import           Database.Beam.Sqlite.Migrate   ( migrationBackend )
import qualified Database.SQLite.Simple        as SQLite
import qualified Options.Applicative           as OA
import           SecretSanta.Backend.KVStore.Database.Class

data Sqlite

instance IsDatabaseBackend Sqlite where

  type BeamBackend Sqlite = Beam.Sqlite
  type BeamBackendM Sqlite = Beam.SqliteM
  runBeam              = SQLite.runBeamSqlite
  beamMigrationBackend = migrationBackend

  dbSettings           = defaultMigratableDbSettings

  type DBOpts Sqlite = FilePath
  parseDBOpts =
    OA.strOption $ mconcat [OA.long "sqlite", OA.metavar "SQLITE_DATABASE"]

  type DBConnection Sqlite = SQLite.Connection
  createDBConnection db = do
    conn <- SQLite.open db
    SQLite.setTrace conn $ Just putStrLn
    pure conn
  closeDBConnection = SQLite.close
  withDBConnection  = SQLite.withConnection
