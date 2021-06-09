{-# OPTIONS_GHC -Wno-orphans #-}
module SecretSanta.DB
  ( SecretSantaDB(..)
  , secretSantaDB
  , dbFile
  , withConn
  , createDB
  , recreateDB
  , BeamC
  , Sqlite
  , SqliteM
  , SQLite.withConnection
  ) where


import "this"    Database.Beam
import           Database.Beam.Sqlite           ( Sqlite
                                                , SqliteM
                                                , runBeamSqliteDebug
                                                )
import           Database.Beam.Sqlite.Migrate   ( migrationBackend )
import qualified Database.SQLite.Simple        as SQLite

import           System.Directory

import           SecretSanta.Data

dbFile :: FilePath
dbFile = "/home/brecht/code/secret-santa-reflex/secretsanta.db"

instance Table InfoTable where
  data PrimaryKey InfoTable f = InfoId (SecretSantaIdT f)
    deriving (Generic, Beamable)
  primaryKey (T2 (k, _)) = InfoId k

instance Table ParticipantTable where
  data PrimaryKey ParticipantTable f = ParticipantId (T2 SecretSantaIdT (C' PName) f)
    deriving (Generic, Beamable)
  primaryKey (T2 (k, Participant {..})) = ParticipantId $ T2 (k, C' pName)

data SecretSantaDB f = SecretSantaDB
  { _secretsantaInfo         :: f (TableEntity InfoTable)
  , _secretsantaParticipants :: f (TableEntity ParticipantTable)
  }
  deriving Generic
deriving anyclass instance Database be SecretSantaDB

checkedSecretSantaDB :: CheckedDatabaseSettings Sqlite SecretSantaDB
checkedSecretSantaDB = defaultMigratableDbSettings

secretSantaDB :: DatabaseSettings Sqlite SecretSantaDB
secretSantaDB = unCheckDatabase checkedSecretSantaDB

createDB :: IO ()
createDB = bracket (SQLite.open dbFile) SQLite.close $ \conn ->
  runBeamSqliteDebug putStrLn conn
    $ createSchema migrationBackend checkedSecretSantaDB

recreateDB :: IO ()
recreateDB = removeFile dbFile >> createDB

withConn :: (SQLite.Connection -> IO ()) -> IO ()
withConn = SQLite.withConnection dbFile

-- brittany-disable-next-binding
type BeamC be
  = ( BeamSqlBackend be
    , FieldsFulfillConstraint
        (BeamSqlBackendCanSerialize be)
        InfoTable
    , FieldsFulfillConstraint
        (BeamSqlBackendCanSerialize be)
        ParticipantTable
    )

