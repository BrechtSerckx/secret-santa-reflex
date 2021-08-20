module SecretSanta.Backend.KVStore.Database
  ( KVStoreDatabase
  , KVStoreTransaction(..)
  , KVStoreConnection(..)
  , KVStoreConfig(..)
  ) where

import qualified Options.Applicative           as OA
import           Polysemy
import           Polysemy.Input
import           Polysemy.Transaction
import           SecretSanta.Backend.KVStore.Class

import qualified Database.SQLite.Simple        as SQLite

data KVStoreDatabase

instance RunKVStoreBackend KVStoreDatabase where
  parseKVStoreOpts =
    fmap ((Proxy @KVStoreDatabase, ) . KVStoreDatabaseOpts)
      . OA.strOption
      $ mconcat [OA.long "sqlite", OA.metavar "SQLITE_DATABASE"]
  data KVStoreTransaction KVStoreDatabase m a
    = KVStoreDatabaseTransaction { unDBTx :: Transaction SQLite.Connection  m a}
  data KVStoreConnection KVStoreDatabase = KVStoreDatabaseConnection SQLite.Connection
  newtype KVStoreConfig KVStoreDatabase = KVStoreDatabaseConfig FilePath
  newtype KVStoreOpts KVStoreDatabase = KVStoreDatabaseOpts FilePath
    deriving IsString via FilePath
  runKVStoreTransaction act = do
    KVStoreDatabaseConnection conn <- input
    startTransaction conn
    eRes <- runTransaction' conn . rewrite unDBTx $ act
    case eRes of
      Left  _ -> rollbackTransaction conn
      Right _ -> endTransaction conn
    pure eRes
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
