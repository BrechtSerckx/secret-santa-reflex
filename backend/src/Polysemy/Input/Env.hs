module Polysemy.Input.Env
  ( runInputEnv
  ) where

import           Polysemy
import           Polysemy.Input

import           Environment

runInputEnv
  :: Member (Embed IO) r => Decoder i -> Sem (Input i ': r) a -> Sem r a
runInputEnv d = interpret $ \case
  Input -> embed @IO $ decode d
