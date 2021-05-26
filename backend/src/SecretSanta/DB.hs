{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module SecretSanta.DB
  ( SecretSantaDB(..)
  , secretSantaDB
  , dbFile
  , runInsertSecretSanta
  , withConn
  , createDB
  )
where


import           Database.Beam
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Sqlite
import           Database.Beam.Sqlite.Migrate
import           Database.Beam.Backend.SQL
import           Database.Beam.Schema.Tables
import           Database.SQLite.Simple
import           SecretSanta.Data
import           Data.Refine
import           Data.Time
import "common"  Text.EmailAddress
import           Text.NonEmpty

data SecretSantaDB f = SecretSantaDB
  { _secretsantaForms :: f (TableEntity (WithPrimaryKeyT Int InfoT))
  , _secretsantaParticipants :: f (TableEntity (WithPrimaryKeyT Int ParticipantT))
  }
  deriving Generic
  deriving anyclass (Database be)

deriving
  via Refined Text NonEmptyText
  instance HasSqlValueSyntax be Text => HasSqlValueSyntax be NonEmptyText
deriving
  via Refined Text NonEmptyText
  instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be NonEmptyText
deriving
  via Refined Text EmailAddress
  instance HasSqlValueSyntax be Text => HasSqlValueSyntax be EmailAddress
deriving
  via Refined Text EmailAddress
  instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be EmailAddress
deriving
  via Refined Double Price
  instance HasSqlValueSyntax be Double => HasSqlValueSyntax be Price
deriving
  via Refined Double Price
  instance HasDefaultSqlDataType be Double => HasDefaultSqlDataType be Price

deriving newtype
  instance HasSqlValueSyntax be Day => HasSqlValueSyntax be Date

instance HasDefaultSqlDataType be Day => HasDefaultSqlDataType be Date where
  defaultSqlDataType Proxy proxy embedded =
    defaultSqlDataType (Proxy @Day) proxy embedded

instance HasSqlValueSyntax be [Char] => HasSqlValueSyntax be Time where
  sqlValueSyntax = autoSqlValueSyntax
instance HasDefaultSqlDataType be [Char] => HasDefaultSqlDataType be Time where
  defaultSqlDataType Proxy proxy embedded =
    defaultSqlDataType (Proxy @[Char]) proxy embedded

instance HasSqlValueSyntax be [Char] => HasSqlValueSyntax be TimeZone where
  sqlValueSyntax = autoSqlValueSyntax
instance HasDefaultSqlDataType be [Char] => HasDefaultSqlDataType be TimeZone where
  defaultSqlDataType Proxy proxy embedded =
    defaultSqlDataType (Proxy @[Char]) proxy embedded

instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be [Char] where
  defaultSqlDataType Proxy proxy embedded =
    defaultSqlDataType (Proxy @Text) proxy embedded

dbFile :: FilePath
dbFile = "/home/brecht/code/secret-santa-reflex/secretsanta.db"

checkedSecretSantaDB :: CheckedDatabaseSettings Sqlite SecretSantaDB
checkedSecretSantaDB = defaultMigratableDbSettings

secretSantaDB :: DatabaseSettings Sqlite SecretSantaDB
secretSantaDB = unCheckDatabase checkedSecretSantaDB

createDB :: IO ()
createDB = bracket (open dbFile) close $ \conn ->
  runBeamSqliteDebug putStrLn conn
    $ createSchema migrationBackend checkedSecretSantaDB

withConn :: (Connection -> IO ()) -> IO ()
withConn = withConnection dbFile

runInsertSecretSanta :: Connection -> Int -> SecretSanta -> IO ()
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
  -> SqlInsert be (WithPrimaryKeyT Int InfoT)
insertInfo db id val =
  insert (_secretsantaForms db) . insertValues . pure $ WithPrimaryKey id val

insertParticipants
  :: DecentBeamBackend be
  => DatabaseSettings be SecretSantaDB
  -> Int
  -> Participants
  -> SqlInsert be (WithPrimaryKeyT Int ParticipantT)
insertParticipants db id ps =
  insert (_secretsantaParticipants db) . insertValues $ WithPrimaryKey id <$> ps


type DecentBeamBackend be
  = ( BeamSqlBackend be
    , FieldsFulfillConstraint
        (BeamSqlBackendCanSerialize be)
        (WithPrimaryKeyT Int InfoT)
    , FieldsFulfillConstraint
        (BeamSqlBackendCanSerialize be)
        (WithPrimaryKeyT Int ParticipantT)
    )
