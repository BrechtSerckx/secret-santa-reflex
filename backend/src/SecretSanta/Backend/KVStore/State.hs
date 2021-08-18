module SecretSanta.Backend.KVStore.State
  ( KVTransaction(..)
  , KVConnection(..)
  , KVConfig(..)
  ) where

import           Polysemy
import           Polysemy.Input
import           SecretSanta.Backend.KVStore

instance RunKVBackend 'KVState where
  newtype KVTransaction 'KVState m a = KVStateTransaction { unStateTx :: NoOp m a}
  data KVConnection 'KVState = KVStateConnection
  data KVConfig 'KVState = KVStateConfig
  runKVTransaction SKVState = runNoOp . rewrite unStateTx
  runKVConnection SKVState = runInputConst KVStateConnection
  runKVConfig SKVState KVStateConfig = runInputConst KVStateConfig
