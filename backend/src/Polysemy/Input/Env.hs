module Polysemy.Input.Env
  ( runInputEnv
  , runInputEnvOnce
  , EnvError(..)
  ) where

import           Polysemy
import           Polysemy.Input
import           Polysemy.Error
import           Polysemy.Operators

import           System.Envy

newtype EnvError = EnvError { unEnvError :: [Char] }

runInputEnv
  :: (FromEnv i, Members '[Embed IO, Error EnvError] r) => Input i ': r @> a -> r @> a
runInputEnv = interpret $ \case
  Input -> do
    eI <- embed @IO decodeEnv
    fromEither $ first EnvError eI

runInputEnvOnce
  :: forall i a r. (FromEnv i, Members '[Embed IO, Error EnvError] r) => Input i ': r @> a -> r @> a
runInputEnvOnce act = do
  eI <- embed @IO $ decodeEnv @i
  interpret (\case
    Input -> fromEither $ first EnvError eI) act
