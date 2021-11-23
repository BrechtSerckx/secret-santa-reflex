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
import           Data.Time                      ( NominalDiffTime )
import "this"    Database.Beam
import           GHC.Show                       ( Show(..) )
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

instance Show AnyDatabaseBackend where
  show = const "AnyDatabaseBackend"

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

data ConnectionPoolOpts = ConnectionPoolOpts
  { connPoolStripes   :: Int
  , connPoolResources :: Int
  , connPoolKeepConn  :: NominalDiffTime
  }

pConnectionPoolOpts :: OA.Parser ConnectionPoolOpts
pConnectionPoolOpts = do
  connPoolStripes <- OA.option OA.auto $ mconcat
    [ OA.long "conn-pool-stripes"
    , OA.metavar "N_STRIPES"
    , OA.showDefault
    , OA.value 1
    ]
  connPoolResources <- OA.option OA.auto $ mconcat
    [ OA.long "max-connnections"
    , OA.metavar "N_RESOURCES"
    , OA.showDefault
    , OA.value 1
    ]
  connPoolKeepConn <- fmap (realToFrac @Double) . OA.option OA.auto $ mconcat
    [ OA.long "conn-pool-keep-conn"
    , OA.metavar "N_SECONDS"
    , OA.showDefault
    , OA.value 0.5
    ]
  pure ConnectionPoolOpts { .. }


instance IsDatabaseBackend db => RunKVStoreBackend (KVStoreDatabase db) where
  parseKVStoreOpts = do
    dbOpts       <- parseDBOpts @db
    connPoolOpts <- pConnectionPoolOpts
    pure KVStoreDatabaseOpts { .. }
  data KVStoreTransaction (KVStoreDatabase db) m a
    = KVStoreDatabaseTransaction { unDBTx :: Transaction (DBConnection db)  m a}
  data KVStoreConnection (KVStoreDatabase db) = KVStoreDatabaseConnection (DBConnection db)
  newtype KVStoreConfig (KVStoreDatabase db) = KVStoreDatabaseConfig (Pool.Pool (DBConnection db))
  data KVStoreOpts (KVStoreDatabase db) = KVStoreDatabaseOpts
    { dbOpts :: DBOpts db
    , connPoolOpts :: ConnectionPoolOpts
    }
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

  runKVStoreConfig KVStoreDatabaseOpts { connPoolOpts = ConnectionPoolOpts {..}, ..} act
    = do
      pool <- embed $ Pool.createPool (createDBConnection @db dbOpts)
                                      (closeDBConnection @db)
                                      connPoolStripes
                                      connPoolKeepConn
                                      connPoolResources
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
