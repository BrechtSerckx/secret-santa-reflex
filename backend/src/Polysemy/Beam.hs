module Polysemy.Beam
  ( Transaction
  , transact
  , runTransactionSqliteDebug
  )
where

import           Polysemy
import           Polysemy.Operators

import           Database.Beam
import           Database.Beam.Sqlite           ( Sqlite
                                                , SqliteM
                                                -- , runBeamSqliteDebug
                                                , runBeamSqlite
                                                )
import qualified Database.SQLite.Simple        as SQLite

data Transaction be bm m a where
  Transact ::(BeamSqlBackend be, MonadBeam be bm) => bm a -> Transaction be bm m a
makeSem ''Transaction

runTransactionSqliteDebug
  :: SQLite.Connection -> Transaction Sqlite SqliteM ': r @> a -> IO ~@ r @> a
runTransactionSqliteDebug conn = interpret $ \case
  Transact act ->
    -- embed . SQLite.withTransaction conn $ runBeamSqliteDebug putStrLn conn act
    embed . SQLite.withTransaction conn $ runBeamSqlite conn act
