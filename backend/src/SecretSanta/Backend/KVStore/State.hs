module SecretSanta.Backend.KVStore.State
  ( KVState
  , KVTransaction(..)
  , KVConnection(..)
  , KVConfig(..)
  ) where

import qualified Options.Applicative           as OA
import           Polysemy
import           Polysemy.Input
import           SecretSanta.Backend.KVStore.Class

data KVState

instance RunKVBackend KVState where
  parseKVConfig =
    OA.flag' (Proxy @KVState, KVStateConfig) $ mconcat [OA.long "in-memory"]
  newtype KVTransaction KVState m a = KVStateTransaction { unStateTx :: NoOp m a}
  data KVConnection KVState = KVStateConnection
  data KVConfig KVState = KVStateConfig
  runKVTransaction = runNoOp . rewrite unStateTx
  runKVConnection  = runInputConst KVStateConnection
  runKVConfig KVStateConfig = runInputConst KVStateConfig
