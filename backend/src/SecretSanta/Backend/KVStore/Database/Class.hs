{-# LANGUAGE AllowAmbiguousTypes #-}
module SecretSanta.Backend.KVStore.Database.Class
  ( IsDatabaseBackend(..)
  , runBeamTransaction
  , createDB
  ) where

import           Control.Exception              ( bracket )
import           Control.Monad.Fail             ( MonadFail )
import           Database.Beam
import           Database.Beam.Migrate.Backend
import qualified Options.Applicative           as OA
import           Polysemy
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
  runBeam
    :: DBConnection db -> BeamBackendM db a -> IO a
  beamMigrationBackend :: BeamMigrationBackend (BeamBackend db) (BeamBackendM db)

  dbSettings :: CheckedDatabaseSettings (BeamBackend db) SecretSantaDB

  type DBOpts db :: Type
  parseDBOpts :: OA.Parser (DBOpts db)

  type DBConnection db :: Type
  createDBConnection :: DBOpts db -> IO (DBConnection db)
  closeDBConnection :: DBConnection db -> IO ()
  withDBConnection
    :: DBOpts db
    -> (DBConnection db -> IO a) -> IO a
  withDBConnection opts = bracket (createDBConnection @db opts) (closeDBConnection @db)



runBeamTransaction
  :: forall db r a
   . IsDatabaseBackend db
  => BeamTransaction (BeamBackend db) (BeamBackendM db) ': r @> a
  -> Transaction (DBConnection db) ': r @> a
runBeamTransaction = runBeamTransaction' $ runBeam @db


createDB
  :: forall db r
   . (IsDatabaseBackend db, Member (Embed IO) r)
  => DBOpts db
  -> r @> ()
createDB db = embed . withDBConnection @db db $ \conn ->
  runBeam @db conn $ createSchema (beamMigrationBackend @db) (dbSettings @db)
