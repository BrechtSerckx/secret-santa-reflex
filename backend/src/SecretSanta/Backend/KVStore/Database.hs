module SecretSanta.Backend.KVStore.Database
  ( KVDatabase, KVTransaction(..)
  , KVConnection(..)
  , KVConfig(..)
  ) where

import           Polysemy
import qualified Options.Applicative           as OA
import           Polysemy.Input
import           Polysemy.Transaction
import           SecretSanta.Backend.KVStore.Class

import qualified Database.SQLite.Simple        as SQLite

data KVDatabase

instance RunKVBackend KVDatabase where
  parseKVConfig = 
        fmap (Proxy @KVDatabase, ) . OA.strOption $ mconcat
          [OA.long "sqlite", OA.metavar "SQLITE_DATABASE"]
  data KVTransaction KVDatabase m a
    = KVDatabaseTransaction { unDBTx :: Transaction SQLite.Connection  m a}
  data KVConnection KVDatabase = KVDatabaseConnection SQLite.Connection
  newtype KVConfig KVDatabase = KVDatabaseConfig FilePath
    deriving IsString via FilePath
  runKVTransaction act = do
    KVDatabaseConnection conn <- input
    startTransaction conn
    eRes <- runTransaction' conn . rewrite unDBTx $ act
    case eRes of
      Left  _ -> rollbackTransaction conn
      Right _ -> endTransaction conn
    pure eRes
  runKVConnection act = do
    KVDatabaseConfig db <- input
    withLowerToIO $ \lowerToIO finalize -> do
      res <- SQLite.withConnection db $ \conn -> do
        SQLite.setTrace conn $ Just putStrLn
        lowerToIO $ runInputConst (KVDatabaseConnection conn) act
      finalize
      pure res
  runKVConfig cfg = runInputConst cfg
