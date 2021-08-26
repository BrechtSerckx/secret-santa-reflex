module SecretSanta.Backend.Database.Sqlite
  ( Sqlite
  ) where

import qualified Database.Beam.Sqlite.Connection
                                               as Beam
import qualified Database.SQLite.Simple        as SQLite
import           SecretSanta.Backend.Database.Class

data Sqlite

instance IsDatabaseBackend Sqlite where
  type DBConnection Sqlite = SQLite.Connection
