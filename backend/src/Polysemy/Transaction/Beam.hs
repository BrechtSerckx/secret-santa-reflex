{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Polysemy.Transaction.Beam
  ( BeamTransaction(..)
  , beamTransact
  , runBeamTransactionSqlite
  , module Export
  ) where

import           Polysemy
import           Polysemy.Operators
import           Polysemy.Transaction          as Export


import           Database.Beam
import           Database.Beam.Sqlite           ( Sqlite
                                                , SqliteM
                                                , runBeamSqlite
                                                )
import qualified Database.SQLite.Simple        as SQLite

-- * Beam Transactions

data BeamTransaction be bm m a where
  BeamTransact ::(BeamSqlBackend be, MonadBeam be bm) => bm a -> BeamTransaction be bm m a
makeSem ''BeamTransaction

runBeamTransactionSqlite
  :: BeamTransaction Sqlite SqliteM ': r @> a
  -> Transaction SQLite.Connection ': r @> a
runBeamTransactionSqlite = reinterpret $ \case
  BeamTransact act -> transact $ \conn -> runBeamSqlite conn act
