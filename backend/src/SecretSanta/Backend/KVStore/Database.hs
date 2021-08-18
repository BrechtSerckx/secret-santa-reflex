module SecretSanta.Backend.KVStore.Database
  ( KVTransaction(..)
  , KVConnection(..)
  , KVConfig(..)
  ) where

import           Polysemy
import           Polysemy.Input
import           Polysemy.Transaction
import           SecretSanta.Backend.KVStore

import qualified Database.SQLite.Simple        as SQLite


instance RunKVBackend 'KVDatabase where
  data KVTransaction 'KVDatabase m a
    = KVDatabaseTransaction { unDBTx :: Transaction SQLite.Connection  m a}
  data KVConnection 'KVDatabase = KVDatabaseConnection SQLite.Connection
  newtype KVConfig 'KVDatabase = KVDatabaseConfig FilePath
    deriving IsString via FilePath
  runKVTransaction SKVDatabase{} act = do
    KVDatabaseConnection conn <- input
    startTransaction conn
    eRes <- runTransaction' conn . rewrite unDBTx $ act
    case eRes of
      Left  _ -> rollbackTransaction conn
      Right _ -> endTransaction conn
    pure eRes
  runKVConnection SKVDatabase{} act = do
    KVDatabaseConfig db <- input
    withLowerToIO $ \lowerToIO finalize -> do
      res <- SQLite.withConnection db $ \conn -> do
        SQLite.setTrace conn $ Just putStrLn
        lowerToIO $ runInputConst (KVDatabaseConnection conn) act
      finalize
      pure res
  runKVConfig SKVDatabase cfg = runInputConst cfg
