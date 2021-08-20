{-# LANGUAGE AllowAmbiguousTypes #-}
module SecretSanta.Backend.KVStore.Class
  ( NoOp
  , noOp
  , runNoOp
  , FoldC
  , RunKVStore(..)
  , RunKVBackend(..)
  ) where

import qualified Options.Applicative           as OA
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

class RunKVBackend kv where
  parseKVOpts :: OA.Parser (Proxy kv, KVOpts kv)
  data KVTransaction kv (m :: Type -> Type) (a :: Type) :: Type
  data KVConnection kv :: Type
  data KVConfig kv :: Type
  data KVOpts kv :: Type

  runKVTransaction
    :: Members '[Embed IO, Input (KVConnection kv)] r
    => KVTransaction kv ': r @> Either e a
    -> r @> Either e a
  runKVConnection
    :: Members '[Embed IO, Input (KVConfig kv)] r
    => Input (KVConnection kv) ': r @> a
    -> r @> a
  runKVConfig
    :: KVOpts kv
    -> Input (KVConfig kv) ': r @> a
    -> r @> a

class RunKVBackend kv => RunKVStore kv store where
  data KVStoreInit kv store (m :: Type -> Type) (a :: Type) :: Type
  runKVStore
    :: Members '[KVTransaction kv, KVStoreInit kv store] r
    => store ': r @> a
    -> r @> a
  runKVStoreInit
    :: KVStoreInit kv store ': r @> a
    -> r @> a
