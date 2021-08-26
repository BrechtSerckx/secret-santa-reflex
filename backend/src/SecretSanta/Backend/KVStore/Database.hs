module SecretSanta.Backend.KVStore.Database
  ( KVStoreDatabase
  , KVStoreTransaction(..)
  , KVStoreConnection(..)
  , KVStoreConfig(..)
  , runKVStore'
  , runKVStoreInit'
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
import           SecretSanta.Backend.KVStore.Class
import           SecretSanta.Database

data KVStoreDatabase

instance RunKVStoreBackend KVStoreDatabase where
  parseKVStoreOpts = fmap KVStoreDatabaseOpts . OA.strOption $ mconcat
    [OA.long "sqlite", OA.metavar "SQLITE_DATABASE"]
  data KVStoreTransaction KVStoreDatabase m a
    = KVStoreDatabaseTransaction { unDBTx :: Transaction SQLite.Connection  m a}
  data KVStoreConnection KVStoreDatabase = KVStoreDatabaseConnection SQLite.Connection
  newtype KVStoreConfig KVStoreDatabase = KVStoreDatabaseConfig FilePath
  newtype KVStoreOpts KVStoreDatabase = KVStoreDatabaseOpts FilePath
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
   . (  forall m a
   . Input (DatabaseSettings Beam.Sqlite SecretSantaDB) m a
  -> KVStoreInit KVStoreDatabase store m a
  )
  -> (  forall m a
      . KVStoreInit KVStoreDatabase store m a
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
       '[ KVStoreTransaction KVStoreDatabase
        , KVStoreInit KVStoreDatabase store
        ]
       r
  => (store ': r @> a -> r @> a)
runKVStore' construct deconstruct runStore =
  subsume @(KVStoreTransaction KVStoreDatabase)
    . rewrite KVStoreDatabaseTransaction
    . subsume @(KVStoreInit KVStoreDatabase store)
    . rewrite construct
    . rotateEffects2
    . runBeamTransactionSqlite
    . runStore
    . raiseUnder
    . rotateEffects2
    . rewrite deconstruct
    . raise

runKVStoreInit'
  :: forall store
   . (  forall m a
   . KVStoreInit KVStoreDatabase store m a
  -> Input (DatabaseSettings Sqlite SecretSantaDB) m a
  )
  -> forall r a . (KVStoreInit KVStoreDatabase store : r @> a -> r @> a)
runKVStoreInit' deconstructor =
  runInputConst secretSantaDB . rewrite deconstructor
