{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module SecretSanta.Backend.KVStore
  ( module Export
  , AnyKVBackendWithConfig(..)
  , parseKVStoreBackends
  , RunKVStores
  ) where

import qualified Options.Applicative           as OA
import           SecretSanta.Backend.KVStore.Class
                                               as Export
import           SecretSanta.Backend.KVStore.Database
                                               as Export
import           SecretSanta.Backend.KVStore.State
                                               as Export
import           SecretSanta.Effect.Store      as Export
import           Type.Constraint                ( FoldC )


type KVStoreBackends = '[KVState , KVDatabase]
type RunKVStores kv = FoldC (RunKVStore kv) Stores

parseKVStoreBackends :: OA.Parser AnyKVBackendWithConfig
parseKVStoreBackends = parseKVStoreBackends' @KVStoreBackends

class ParseKVStoreBackends kvs where
  parseKVStoreBackends' :: OA.Parser AnyKVBackendWithConfig

instance ParseKVStoreBackends '[] where
  parseKVStoreBackends' = OA.empty
instance (RunKVBackend kv, RunKVStores kv, ParseKVStoreBackends kvs) => ParseKVStoreBackends (kv ': kvs) where
  parseKVStoreBackends' =
    (AnyKVBackendWithConfig <$> parseKVOpts @kv) <|> parseKVStoreBackends' @kvs

data AnyKVBackendWithConfig where
  AnyKVBackendWithConfig
    ::(RunKVBackend kv, RunKVStores kv)
    => (Proxy kv , KVOpts kv)
    -> AnyKVBackendWithConfig
