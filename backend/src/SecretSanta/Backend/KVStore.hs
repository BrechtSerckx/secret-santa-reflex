{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module SecretSanta.Backend.KVStore
  ( module Export
  , AnyKVStoreBackend(..)
  , parseKVStoreBackends
  , RunKVStores
  )
where

import           Data.Constraint                ( FoldC )
import           Data.Type
import qualified Options.Applicative           as OA
import           SecretSanta.Backend.KVStore.Class
                                               as Export
import           SecretSanta.Backend.KVStore.Database
                                               as Export
import           SecretSanta.Backend.KVStore.State
                                               as Export
import           SecretSanta.Effect.Store      as Export


type KVStoreBackends
  = '[KVStoreState ] :++ TMap KVStoreDatabase DatabaseBackends
type RunKVStores kv = FoldC (RunKVStore kv) Stores

parseKVStoreBackends :: OA.Parser AnyKVStoreBackend
parseKVStoreBackends = parseKVStoreBackends' @KVStoreBackends

class ParseKVStoreBackends kvs where
  parseKVStoreBackends' :: OA.Parser AnyKVStoreBackend

instance ParseKVStoreBackends '[] where
  parseKVStoreBackends' = OA.empty
instance (RunKVStoreBackend kv, RunKVStores kv, ParseKVStoreBackends kvs) => ParseKVStoreBackends (kv ': kvs) where
  parseKVStoreBackends' =
    (AnyKVStoreBackend <$> parseKVStoreOpts @kv) <|> parseKVStoreBackends' @kvs

data AnyKVStoreBackend where
  AnyKVStoreBackend
    ::(RunKVStoreBackend kv, RunKVStores kv)
    => KVStoreOpts kv
    -> AnyKVStoreBackend
