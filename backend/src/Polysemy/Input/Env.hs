module Polysemy.Input.Env
  ( runInputEnv
  , runInputEnvOnce
  , EnvError(..)
  ) where

import           Polysemy
import           Polysemy.Input
import           Polysemy.Operators

import           Control.Exception              ( throwIO )
import           System.Envy

newtype EnvError = EnvError { unEnvError :: [Char] }
  deriving newtype Show
  deriving anyclass Exception

runInputEnv :: (FromEnv i, Member (Embed IO) r) => Input i ': r @> a -> r @> a
runInputEnv = interpret $ \case
  Input -> do
    eI <- embed @IO decodeEnv
    case eI of
      Left  e -> embed @IO . throwIO $ EnvError e
      Right i -> pure i

runInputEnvOnce
  :: forall i a r
   . (FromEnv i, Member (Embed IO) r)
  => Input i ': r @> a
  -> r @> a
runInputEnvOnce act = do
  eI <- embed @IO $ decodeEnv @i
  case eI of
    Left  e -> embed @IO . throwIO $ EnvError e
    Right i -> interpret
      (\case
        Input -> pure i
      )
      act
