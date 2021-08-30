{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Polysemy.Transaction.Beam
  ( BeamTransaction(..)
  , beamTransact
  , runBeamTransaction'
  , module Export
  ) where

import           Database.Beam
import           Polysemy
import           Polysemy.Operators
import           Polysemy.Transaction          as Export

-- * Beam Transactions

data BeamTransaction be bm m a where
  BeamTransact ::(BeamSqlBackend be, MonadBeam be bm) => bm a -> BeamTransaction be bm m a
makeSem ''BeamTransaction

runBeamTransaction'
  :: (c -> forall x . m x -> IO x)
  -> BeamTransaction be m ': r @> a
  -> Transaction c ': r @> a
runBeamTransaction' f = reinterpret $ \case
  BeamTransact act -> transact $ \conn -> f conn act
