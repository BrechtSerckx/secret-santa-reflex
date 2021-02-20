module SecretSanta.Effect.SecretSanta
  ( SecretSanta
  , createSecretSanta
  , runSecretSantaPrint
  ) where

import           Polysemy

import SecretSanta.Data

data SecretSanta m a where
  -- | Create a new secret santa
  CreateSecretSanta ::Form -> SecretSanta m ()

makeSem ''SecretSanta

runSecretSantaPrint
  :: forall r a. Sem (SecretSanta ': r) a -> Sem (Embed IO ': r) a
runSecretSantaPrint =
  reinterpret \case
    CreateSecretSanta f -> embed $ print @IO f
