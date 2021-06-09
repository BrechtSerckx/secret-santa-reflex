{-# OPTIONS_GHC -Wno-orphans #-}
module SecretSanta.DB
  ( SecretSantaDB(..)
  , secretSantaDB
  , dbFile
  , withConn
  , createDB
  , Transaction
  , transact
  , DecentBeamBackend
  , runTransactionSqliteDebug
  , SqliteM
  , SQLite.withConnection
  )
where


import           Polysemy
import           Polysemy.Operators

import           Database.Beam
import           Database.Beam.Backend          ( BeamSqlBackend
                                                , BeamSqlBackendCanSerialize
                                                )
import           Database.Beam.Migrate          ( CheckedDatabaseSettings
                                                , defaultMigratableDbSettings
                                                , unCheckDatabase
                                                )
import           Database.Beam.Migrate.Simple   ( createSchema )
import           Database.Beam.Schema.Tables    ( FieldsFulfillConstraint )
import           Database.Beam.Sqlite           ( Sqlite
                                                , runBeamSqliteDebug
                                                , SqliteM
                                                )
import           Database.Beam.Sqlite.Migrate   ( migrationBackend )
import           Database.Beam.T2               ( T2(..) )
import qualified Database.SQLite.Simple        as SQLite
import           SecretSanta.Data

dbFile :: FilePath
dbFile = "/home/brecht/code/secret-santa-reflex/secretsanta.db"

newtype C' a f = C' (C f a)
  deriving stock Generic
  deriving anyclass Beamable

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

withConn :: (SQLite.Connection -> IO ()) -> IO ()
withConn = SQLite.withConnection dbFile

-- brittany-disable-next-binding
type DecentBeamBackend be
  = ( BeamSqlBackend be
    , FieldsFulfillConstraint
        (BeamSqlBackendCanSerialize be)
        InfoTable
    , FieldsFulfillConstraint
        (BeamSqlBackendCanSerialize be)
        ParticipantTable
    )

data Transaction bm m a where
  Transact ::(DecentBeamBackend be, MonadBeam be bm) => bm a -> Transaction  bm m a
makeSem ''Transaction

runTransactionSqliteDebug
  :: SQLite.Connection -> Transaction SqliteM ': r @> a -> IO ~@ r @> a
runTransactionSqliteDebug conn = interpret $ \case
  Transact act -> embed $ runBeamSqliteDebug putStrLn conn act
