module SecretSanta.Backend.KVStore.Database
  ( KVStoreDatabase
  , KVStoreTransaction(..)
  , KVStoreConnection(..)
  , KVStoreConfig(..)
  , runKVStore'
  , runKVStoreInit'
  , module Export
  )
where

import "this"    Database.Beam
import           Polysemy
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Operators
import           Polysemy.Transaction
import           Polysemy.Transaction.Beam
import           SecretSanta.Backend.Database.Class
                                               as Export
import           SecretSanta.Backend.KVStore.Class
                                               as Export
import           SecretSanta.Database

data KVStoreDatabase db

instance IsDatabaseBackend db => RunKVStoreBackend (KVStoreDatabase db) where
  parseKVStoreOpts = KVStoreDatabaseOpts <$> parseConn @(DBConnection db)
  data KVStoreTransaction (KVStoreDatabase db) m a
    = KVStoreDatabaseTransaction { unDBTx :: Transaction (DBConnection db)  m a}
  data KVStoreConnection (KVStoreDatabase db) = KVStoreDatabaseConnection (DBConnection db)
  newtype KVStoreConfig (KVStoreDatabase db) = KVStoreDatabaseConfig (WithConnectionInput (DBConnection db))
  newtype KVStoreOpts (KVStoreDatabase db) = KVStoreDatabaseOpts (WithConnectionInput (DBConnection db))
  runKVStoreTransaction act = do
    KVStoreDatabaseConnection conn <- input
    runTransaction conn . rewrite unDBTx $ act
  runKVStoreConnection act = do
    KVStoreDatabaseConfig db <- input
    withLowerToIO $ \lowerToIO finalize -> do
      res <- withConnection db $ \conn -> do
        lowerToIO $ runInputConst (KVStoreDatabaseConnection conn) act
      finalize
      pure res
  runKVStoreConfig (KVStoreDatabaseOpts cfg) =
    runInputConst $ KVStoreDatabaseConfig cfg

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
       '[KVStoreTransaction (KVStoreDatabase db), KVStoreInit
         (KVStoreDatabase db)
         store]
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
   . (  forall m a
   . KVStoreInit (KVStoreDatabase db) store m a
  -> Input (DatabaseSettings (BeamBackend db) SecretSantaDB) m a
  )
  -> forall r a . (KVStoreInit (KVStoreDatabase db) store : r @> a -> r @> a)
runKVStoreInit' deconstructor =
  runInputConst secretSantaDB . rewrite deconstructor
