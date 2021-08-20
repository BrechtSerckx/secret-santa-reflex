module SecretSanta.Backend.KVStore.State
  ( KVStoreState
  , KVStoreTransaction(..)
  , KVStoreConnection(..)
  , KVStoreConfig(..)
  ) where

import qualified Options.Applicative           as OA
import           Polysemy
import           Polysemy.Input
import           Polysemy.NoOp
import           SecretSanta.Backend.KVStore.Class

data KVStoreState

instance RunKVStoreBackend KVStoreState where
  parseKVStoreOpts = OA.flag' KVStoreStateOpts $ mconcat [OA.long "in-memory"]
  newtype KVStoreTransaction KVStoreState m a = KVStoreStateTransaction { unStateTx :: NoOp m a}
  data KVStoreConnection KVStoreState = KVStoreStateConnection
  data KVStoreConfig KVStoreState = KVStoreStateConfig
  data KVStoreOpts KVStoreState = KVStoreStateOpts
  runKVStoreTransaction = runNoOp . rewrite unStateTx
  runKVStoreConnection  = runInputConst KVStoreStateConnection
  runKVStoreConfig KVStoreStateOpts = runInputConst KVStoreStateConfig
