module SecretSanta.Backend.KVStore.State
  ( KVState
  , KVTransaction(..)
  , KVConnection(..)
  , KVConfig(..)
  ) where

import qualified Options.Applicative           as OA
import           Polysemy
import           Polysemy.Input
import           Polysemy.NoOp
import           SecretSanta.Backend.KVStore.Class

data KVState

instance RunKVBackend KVState where
  parseKVOpts =
    OA.flag' (Proxy @KVState, KVStateOpts) $ mconcat [OA.long "in-memory"]
  newtype KVTransaction KVState m a = KVStateTransaction { unStateTx :: NoOp m a}
  data KVConnection KVState = KVStateConnection
  data KVConfig KVState = KVStateConfig
  data KVOpts KVState = KVStateOpts
  runKVTransaction = runNoOp . rewrite unStateTx
  runKVConnection  = runInputConst KVStateConnection
  runKVConfig KVStateOpts = runInputConst KVStateConfig
