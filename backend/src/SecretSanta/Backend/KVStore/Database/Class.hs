{-# LANGUAGE AllowAmbiguousTypes #-}
module SecretSanta.Backend.KVStore.Database.Class
  ( IsDatabaseBackend(..)
  , runBeamTransaction
  )
where

import           Database.Beam
import           Polysemy.Input
import           Polysemy.Operators
import           Polysemy.Transaction
import           Polysemy.Transaction.Beam
import           SecretSanta.Database
import qualified Options.Applicative           as OA

class
  ( CanTransact (DBConnection db)
  , BeamC (BeamBackend db)
  , MonadBeam (BeamBackend db) (BeamBackendM db)
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

  createDB :: DBOpts db -> IO ()

runBeamTransaction
  :: forall db r a
   . IsDatabaseBackend db
  => BeamTransaction (BeamBackend db) (BeamBackendM db) ': r @> a
  -> Transaction (DBConnection db) ': r @> a
runBeamTransaction = runBeamTransaction' $ runBeam @db
