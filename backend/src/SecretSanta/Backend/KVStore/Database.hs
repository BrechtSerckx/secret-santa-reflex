module SecretSanta.Backend.KVStore.Database
  ( KVStoreDatabase
  , KVStoreTransaction(..)
  , KVStoreConnection(..)
  , KVStoreConfig(..)
  , runKVStore'
  , runKVStoreInit'
  , module Export
  ) where

import "this"    Database.Beam
import qualified Database.Beam.Sqlite.Connection
                                               as Beam
import qualified Database.SQLite.Simple        as SQLite
import qualified Options.Applicative           as OA
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
  parseKVStoreOpts = fmap KVStoreDatabaseOpts . OA.strOption $ mconcat
    [OA.long "sqlite", OA.metavar "SQLITE_DATABASE"]
  data KVStoreTransaction (KVStoreDatabase db) m a
    = KVStoreDatabaseTransaction { unDBTx :: Transaction SQLite.Connection  m a}
  data KVStoreConnection (KVStoreDatabase db) = KVStoreDatabaseConnection SQLite.Connection
  newtype KVStoreConfig (KVStoreDatabase db) = KVStoreDatabaseConfig FilePath
  newtype KVStoreOpts (KVStoreDatabase db) = KVStoreDatabaseOpts FilePath
    deriving IsString via FilePath
  runKVStoreTransaction act = do
    KVStoreDatabaseConnection conn <- input
    runTransaction conn . rewrite unDBTx $ act
  runKVStoreConnection act = do
    KVStoreDatabaseConfig db <- input
    withLowerToIO $ \lowerToIO finalize -> do
      res <- SQLite.withConnection db $ \conn -> do
        SQLite.setTrace conn $ Just putStrLn
        lowerToIO $ runInputConst (KVStoreDatabaseConnection conn) act
      finalize
      pure res
  runKVStoreConfig (KVStoreDatabaseOpts cfg) =
    runInputConst $ KVStoreDatabaseConfig cfg

-- | helper for implementing `runKVStore`
runKVStore'
  :: forall
       store
       db
   . (  forall m a
   . Input (DatabaseSettings Beam.Sqlite SecretSantaDB) m a
  -> KVStoreInit (KVStoreDatabase db) store m a
  )
  -> (  forall m a
      . KVStoreInit (KVStoreDatabase db) store m a
     -> Input (DatabaseSettings Beam.Sqlite SecretSantaDB) m a
     )
  -> ( forall
         a
         r
      . store ': BeamTransaction Sqlite SqliteM ': Input (DatabaseSettings Sqlite SecretSantaDB) ': r @> a
     -> BeamTransaction Sqlite SqliteM ': Input (DatabaseSettings Sqlite SecretSantaDB) ': r @> a
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
    . runBeamTransactionSqlite
    . runStore
    . raiseUnder
    . rotateEffects2
    . rewrite deconstruct
    . raise

runKVStoreInit'
  :: forall store db
   . (  forall m a
   . KVStoreInit (KVStoreDatabase db) store m a
  -> Input (DatabaseSettings Sqlite SecretSantaDB) m a
  )
  -> forall r a . (KVStoreInit (KVStoreDatabase db) store : r @> a -> r @> a)
runKVStoreInit' deconstructor =
  runInputConst secretSantaDB . rewrite deconstructor
