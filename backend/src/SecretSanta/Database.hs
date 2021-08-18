{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module SecretSanta.Database where

import           Data.String                    ( String )
import           Polysemy
import           Polysemy.Input
import           Polysemy.Operators
import           Polysemy.Transaction

import qualified Database.SQLite.Simple        as SQLite
import SecretSanta.DB

import "this"    Database.Beam
import qualified Database.Beam.Sqlite.Connection
                                               as Beam

data NoOp m a where
  NoOp ::a -> NoOp m a
makeSem ''NoOp

runNoOp :: NoOp ': r @> a -> r @> a
runNoOp = interpret \case
  NoOp a -> pure a

type family FoldC mkC as :: Constraint where
  FoldC mkC '[] = ()
  FoldC mkC (a ': as) = (mkC a, FoldC mkC as)

data KVBackend = KVState | KVDatabase
  deriving (Eq, Show, Read)

data SKVBackend kv where
  SKVState ::SKVBackend 'KVState
  SKVDatabase ::SKVBackend 'KVDatabase
data AnyKVBackendWithConfig stores where
  AnyKVBackendWithConfig
    :: (RunKVBackend kv, FoldC (RunKVStore kv) stores)
    => SKVBackend kv
    -> KVConfig kv
    -> AnyKVBackendWithConfig stores

class RunKVBackend (kv:: KVBackend) where
  data KVTransaction kv (m :: Type -> Type) (a :: Type) :: Type
  data KVConnection kv :: Type
  data KVConfig kv :: Type
  runKVTransaction
    :: Members '[Embed IO, Input (KVConnection kv)] r
    => SKVBackend kv
    -> KVTransaction kv ': r @> Either e a
    -> r @> Either e a
  runKVConnection
    :: Members '[Embed IO, Input (KVConfig kv)] r
    => SKVBackend kv
    -> Input (KVConnection kv) ': r @> a
    -> r @> a
  runKVConfig
    :: SKVBackend kv
    -> KVConfig kv
    -> Input (KVConfig kv) ': r @> a
    -> r @> a

instance RunKVBackend 'KVState where
  newtype KVTransaction 'KVState m a = KVStateTransaction { unStateTx :: NoOp m a}
  data KVConnection 'KVState = KVStateConnection
  data KVConfig 'KVState = KVStateConfig
  runKVTransaction SKVState = runNoOp . rewrite unStateTx
  runKVConnection SKVState =
    runInputConst KVStateConnection
  runKVConfig SKVState KVStateConfig = runInputConst KVStateConfig

instance RunKVBackend 'KVDatabase where
  data KVTransaction 'KVDatabase m a
    = KVDatabaseTransaction { unDBTx :: Transaction SQLite.Connection  m a}
  data KVConnection 'KVDatabase = KVDatabaseConnection SQLite.Connection
  newtype KVConfig 'KVDatabase = KVDatabaseConfig FilePath
    deriving IsString via FilePath
  runKVTransaction SKVDatabase{} act = do
    KVDatabaseConnection conn <- input
    startTransaction conn
    eRes <- runTransaction' conn  . rewrite unDBTx $ act
    case eRes of
      Left  _ -> rollbackTransaction conn
      Right _ -> endTransaction conn
    pure eRes
  runKVConnection SKVDatabase{} act = do
    KVDatabaseConfig db <- input
    withLowerToIO $ \lowerToIO finalize -> do
      res <- SQLite.withConnection db
        $ \conn -> do
        SQLite.setTrace conn $ Just putStrLn
        lowerToIO $ runInputConst (KVDatabaseConnection conn) act
      finalize
      pure res
  runKVConfig SKVDatabase cfg = runInputConst cfg

class RunKVBackend kv => RunKVStore kv store where
  data KVStoreInit kv store (m :: Type -> Type) (a :: Type) :: Type
  runKVStore
    :: Members '[KVTransaction kv, KVStoreInit kv store] r
    => SKVBackend kv
    -> store ': r @> a
    -> r @> a
  runKVStoreInit
    :: SKVBackend kv
    -> KVStoreInit kv store ': r @> a
    -> r @> a
