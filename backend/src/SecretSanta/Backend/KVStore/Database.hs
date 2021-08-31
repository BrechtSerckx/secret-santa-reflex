{-# LANGUAGE AllowAmbiguousTypes #-}
module SecretSanta.Backend.KVStore.Database
  ( KVStoreDatabase
  , DatabaseBackends
  , AnyDatabaseBackend(..)
  , KVStoreTransaction(..)
  , KVStoreConnection(..)
  , KVStoreConfig(..)
  , runKVStore'
  , runKVStoreInit'
  , parseDatabaseBackends
  , module Export
  ) where

import qualified Data.Pool                     as Pool
import "this"    Database.Beam
-- import           SecretSanta.Backend.KVStore.Database.Postgres
--                                                as Export
import qualified Options.Applicative           as OA
import           Polysemy
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Operators
import           Polysemy.Transaction
import           Polysemy.Transaction.Beam
import           SecretSanta.Backend.KVStore.Class
                                               as Export
import           SecretSanta.Backend.KVStore.Database.Class
                                               as Export
import           SecretSanta.Backend.KVStore.Database.Sqlite
                                               as Export
import           SecretSanta.Database

data KVStoreDatabase db
type DatabaseBackends = '[Sqlite]

data AnyDatabaseBackend where
  AnyDatabaseBackend ::IsDatabaseBackend db => Proxy db -> DBOpts db -> AnyDatabaseBackend

parseDatabaseBackends :: OA.Parser AnyDatabaseBackend
parseDatabaseBackends = parseDatabaseBackends' @DatabaseBackends

class ParseDatabaseBackends dbs where
  parseDatabaseBackends' :: OA.Parser AnyDatabaseBackend

instance ParseDatabaseBackends '[] where
  parseDatabaseBackends' = OA.empty
instance (IsDatabaseBackend db, ParseDatabaseBackends dbs) => ParseDatabaseBackends (db ': dbs) where
  parseDatabaseBackends' =
    (AnyDatabaseBackend (Proxy @db) <$> parseDBOpts @db)
      <|> parseDatabaseBackends' @dbs


instance IsDatabaseBackend db => RunKVStoreBackend (KVStoreDatabase db) where
  parseKVStoreOpts = KVStoreDatabaseOpts <$> parseDBOpts @db
  data KVStoreTransaction (KVStoreDatabase db) m a
    = KVStoreDatabaseTransaction { unDBTx :: Transaction (DBConnection db)  m a}
  data KVStoreConnection (KVStoreDatabase db) = KVStoreDatabaseConnection (DBConnection db)
  newtype KVStoreConfig (KVStoreDatabase db) = KVStoreDatabaseConfig (Pool.Pool (DBConnection db))
  newtype KVStoreOpts (KVStoreDatabase db) = KVStoreDatabaseOpts (DBOpts db)
  runKVStoreTransaction act = do
    KVStoreDatabaseConnection conn <- input
    runTransaction conn . rewrite unDBTx $ act
  runKVStoreConnection act = do
    KVStoreDatabaseConfig pool <- input
    withLowerToIO $ \lowerToIO finalize -> do
      res <- Pool.withResource pool $ \conn -> do
        lowerToIO $ runInputConst (KVStoreDatabaseConnection conn) act
      finalize
      pure res

  runKVStoreConfig (KVStoreDatabaseOpts opts) act = do
    let nStripes   = 1
        nResources = 1
        keepConn   = 0.5
    pool <- embed $ Pool.createPool (createDBConnection @db opts)
                                    (closeDBConnection @db)
                                    nStripes
                                    keepConn
                                    nResources
    runInputConst (KVStoreDatabaseConfig pool) act

-- | helper for implementing `runKVStore`
runKVStore'
  :: forall store db
   . IsDatabaseBackend db
  => (  forall m a
      . Input (DatabaseSettings (BeamBackend db) SecretSantaDB) m a
     -> KVStoreInit (KVStoreDatabase db) store m a
     )
  -> (  forall m a
      . KVStoreInit (KVStoreDatabase db) store m a
     -> Input (DatabaseSettings (BeamBackend db) SecretSantaDB) m a
     )
  -> ( forall
         a
         r
      . store ': BeamTransaction (BeamBackend db) (BeamBackendM db) ': Input (DatabaseSettings (BeamBackend db) SecretSantaDB) ': r @> a
     -> BeamTransaction (BeamBackend db) (BeamBackendM db) ': Input (DatabaseSettings (BeamBackend db) SecretSantaDB) ': r @> a
     )
  -> forall a r
   . Members
       '[ KVStoreTransaction (KVStoreDatabase db)
        , KVStoreInit (KVStoreDatabase db) store
        ]
       r
  => (store ': r @> a -> r @> a)
runKVStore' construct deconstruct runStore =
  subsume @(KVStoreTransaction (KVStoreDatabase db))
    . rewrite KVStoreDatabaseTransaction
    . subsume @(KVStoreInit (KVStoreDatabase db) store)
    . rewrite construct
    . rotateEffects2
    . runBeamTransaction @db
    . runStore
    . raiseUnder
    . rotateEffects2
    . rewrite deconstruct
    . raise

runKVStoreInit'
  :: forall store db
   . IsDatabaseBackend db
  => (  forall m a
      . KVStoreInit (KVStoreDatabase db) store m a
     -> Input (DatabaseSettings (BeamBackend db) SecretSantaDB) m a
     )
  -> forall r a
   . (KVStoreInit (KVStoreDatabase db) store : r @> a -> r @> a)
runKVStoreInit' deconstructor =
  runInputConst (unCheckDatabase $ dbSettings @db) . rewrite deconstructor
