module SecretSanta.Backend.KVStore (NoOp, noOp, runNoOp, FoldC, RunKVStore(..), KVBackend(..),SKVBackend(..), AnyKVBackendWithConfig(..), RunKVBackend(..)) where

import           Polysemy
import           Polysemy.Input
import           Polysemy.Operators

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
