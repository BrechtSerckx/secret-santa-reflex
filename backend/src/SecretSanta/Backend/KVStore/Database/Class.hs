{-# LANGUAGE AllowAmbiguousTypes #-}
module SecretSanta.Backend.KVStore.Database.Class
  ( IsDatabaseBackend(..)
  , runBeamTransaction
  , createDB
  ) where

import           Control.Monad.Fail             ( MonadFail )
import           Database.Beam
import           Database.Beam.Migrate.Backend
import qualified Options.Applicative           as OA
import           Polysemy
import           Polysemy.Input
import           Polysemy.Operators
import           Polysemy.Transaction
import           Polysemy.Transaction.Beam
import           SecretSanta.Database

class
  ( CanTransact (DBConnection db)
  , BeamC (BeamBackend db)
  , MonadBeam (BeamBackend db) (BeamBackendM db)
  , MonadFail (BeamBackendM db)
  ) => IsDatabaseBackend db where

  type BeamBackend db :: Type

  type BeamBackendM db :: Type -> Type

  type DBConnection db :: Type
  type DBConfig db :: Type
  type DBOpts db :: Type

  runBeam
    :: DBConnection db -> BeamBackendM db a -> IO a

  withDBConnection
    :: DBConfig db
    -> (DBConnection db -> IO a) -> IO a

  runDBConfig
    :: DBOpts db -> Input (DBConfig db) ': r @> a -> r @> a

  parseDBOpts :: OA.Parser (DBOpts db)

  dbSettings :: CheckedDatabaseSettings (BeamBackend db) SecretSantaDB

  beamMigrationBackend :: BeamMigrationBackend (BeamBackend db) (BeamBackendM db)

runBeamTransaction
  :: forall db r a
   . IsDatabaseBackend db
  => BeamTransaction (BeamBackend db) (BeamBackendM db) ': r @> a
  -> Transaction (DBConnection db) ': r @> a
runBeamTransaction = runBeamTransaction' $ runBeam @db


createDB
  :: forall db r
   . (IsDatabaseBackend db, Member (Embed IO) r)
  => Input (DBConfig db) ': r @> ()
createDB = do
  db <- input
  embed . withDBConnection @db db $ \conn -> runBeam @db conn
    $ createSchema (beamMigrationBackend @db) (dbSettings @db)
