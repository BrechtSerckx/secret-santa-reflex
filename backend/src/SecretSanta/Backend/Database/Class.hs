{-# LANGUAGE AllowAmbiguousTypes #-}
module SecretSanta.Backend.Database.Class
  ( IsDatabaseBackend(..)
  ) where

import           Database.Beam
import           Polysemy.Operators
import           Polysemy.Transaction
import           Polysemy.Transaction.Beam
import           SecretSanta.Database

class
  ( Connection (DBConnection db)
  , BeamC (BeamBackend db)
  , MonadBeam (BeamBackend db) (BeamBackendM db)
  ) => IsDatabaseBackend db where

  type DBConnection db :: Type

  type BeamBackend db :: Type

  type BeamBackendM db :: Type -> Type

  runBeamTransaction
    :: BeamTransaction (BeamBackend db) (BeamBackendM db) ': r @> a
    -> Transaction (DBConnection db) ': r @> a
