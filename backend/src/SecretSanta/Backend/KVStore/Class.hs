{-# LANGUAGE AllowAmbiguousTypes #-}
module SecretSanta.Backend.KVStore.Class
  ( RunKVStoreBackend(..)
  , RunKVStore(..)
  ) where

import qualified Options.Applicative           as OA
import           Polysemy
import           Polysemy.Input
import           Polysemy.Operators

class RunKVStoreBackend kv where
  parseKVStoreOpts :: OA.Parser (KVStoreOpts kv)
  data KVStoreTransaction kv (m :: Type -> Type) (a :: Type) :: Type
  data KVStoreConnection kv :: Type
  data KVStoreConfig kv :: Type
  data KVStoreOpts kv :: Type

  runKVStoreTransaction
    :: Members '[Embed IO, Input (KVStoreConnection kv)] r
    => KVStoreTransaction kv ': r @> Either e a
    -> r @> Either e a
  runKVStoreConnection
    :: Members '[Embed IO, Input (KVStoreConfig kv)] r
    => Input (KVStoreConnection kv) ': r @> a
    -> r @> a
  runKVStoreConfig
    :: Member (Embed IO) r => KVStoreOpts kv
    -> Input (KVStoreConfig kv) ': r @> a
    -> r @> a

class RunKVStoreBackend kv => RunKVStore kv store where
  data KVStoreInit kv store (m :: Type -> Type) (a :: Type) :: Type
  runKVStore
    :: Members '[KVStoreTransaction kv, KVStoreInit kv store] r
    => store ': r @> a
    -> r @> a
  runKVStoreInit
    :: KVStoreInit kv store ': r @> a
    -> r @> a
