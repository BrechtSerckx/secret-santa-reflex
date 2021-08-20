{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module SecretSanta.Backend.KVStore
  ( module Export
  , AnyKVBackendWithConfig(..)
  , parseKVStoreBackends
  ) where

import qualified Options.Applicative           as OA
import           SecretSanta.Backend.KVStore.Class
                                               as Export
import           SecretSanta.Backend.KVStore.Database
                                               as Export
import           SecretSanta.Backend.KVStore.State
                                               as Export
import           SecretSanta.Store             as Export


type KVStoreBackends = '[KVState , KVDatabase]

parseKVStoreBackends :: OA.Parser AnyKVBackendWithConfig
parseKVStoreBackends = parseKVStoreBackends' @KVStoreBackends

class ParseKVStoreBackends kvs where
  parseKVStoreBackends' :: OA.Parser AnyKVBackendWithConfig

instance ParseKVStoreBackends '[] where
  parseKVStoreBackends' = OA.empty
instance (RunKVBackend kv, FoldC (RunKVStore kv) Stores, ParseKVStoreBackends kvs) => ParseKVStoreBackends (kv ': kvs) where
  parseKVStoreBackends' =
    (AnyKVBackendWithConfig <$> parseKVConfig @kv)
      <|> parseKVStoreBackends' @kvs

data AnyKVBackendWithConfig where
  AnyKVBackendWithConfig
    ::(RunKVBackend kv, FoldC (RunKVStore kv) Stores)
    => (Proxy kv , KVConfig kv)
    -> AnyKVBackendWithConfig
