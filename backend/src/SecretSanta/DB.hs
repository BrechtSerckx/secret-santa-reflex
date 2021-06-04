module SecretSanta.DB
  ( SecretSantaDB(..)
  , secretSantaDB
  , dbFile
  , runInsertSecretSanta
  , withConn
  , createDB
  )
where


import           Data.WithId
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
                                                )
import           Database.Beam.Sqlite.Migrate   ( migrationBackend )
import qualified Database.SQLite.Simple        as SQLite
import           SecretSanta.Data

dbFile :: FilePath
dbFile = "/home/brecht/code/secret-santa-reflex/secretsanta.db"

data SecretSantaDB f = SecretSantaDB
  { _secretsantaForms        :: f (TableEntity (WithIdT IntT InfoT))
  , _secretsantaParticipants :: f (TableEntity (WithIdT IntT ParticipantT))
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

runInsertSecretSanta :: SQLite.Connection -> Int -> SecretSanta -> IO ()
runInsertSecretSanta conn id ss =
  let db = secretSantaDB
  in  runBeamSqliteDebug putStrLn conn $ insertSecretSanta db id ss

insertSecretSanta
  :: (DecentBeamBackend be, MonadBeam be m)
  => DatabaseSettings be SecretSantaDB
  -> Int
  -> SecretSanta
  -> m ()
insertSecretSanta db id (SecretSanta UnsafeSecretSanta {..}) = do
  runInsert $ insertInfo db id secretsantaInfo
  runInsert $ insertParticipants db id secretsantaParticipants

insertInfo
  :: DecentBeamBackend be
  => DatabaseSettings be SecretSantaDB
  -> Int
  -> Info
  -> SqlInsert be (WithIdT IntT InfoT)
insertInfo db id val =
  insert (_secretsantaForms db) . insertValues . pure $ WithId (IntT id) val

insertParticipants
  :: DecentBeamBackend be
  => DatabaseSettings be SecretSantaDB
  -> Int
  -> Participants
  -> SqlInsert be (WithIdT IntT ParticipantT)
insertParticipants db id ps =
  insert (_secretsantaParticipants db) . insertValues $ WithId (IntT id) <$> ps


-- brittany-disable-next-binding
type DecentBeamBackend be
  = ( BeamSqlBackend be
    , FieldsFulfillConstraint
        (BeamSqlBackendCanSerialize be)
        (WithIdT IntT InfoT)
    , FieldsFulfillConstraint
        (BeamSqlBackendCanSerialize be)
        (WithIdT IntT ParticipantT)
    )
