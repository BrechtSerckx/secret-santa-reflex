module SecretSanta.Effect.SecretSanta
  ( SecretSanta
  , createSecretSanta
  , runSecretSantaPrint
  , runSecretSanta
  ) where

import           Polysemy

import           Refined

import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.Match

data SecretSanta m a where
  -- | Create a new secret santa
  CreateSecretSanta ::Form -> SecretSanta m ()

makeSem ''SecretSanta

runSecretSantaPrint
  :: forall r a . Sem (SecretSanta ': r) a -> Sem (Embed IO ': r) a
runSecretSantaPrint = reinterpret $ \case
  CreateSecretSanta f -> embed $ print @IO f

runSecretSanta :: forall r a . Sem (SecretSanta ': r) a -> Sem (Match ': Email ': r) a
runSecretSanta = reinterpret2 $ \case
  CreateSecretSanta Form {..} -> do
    mMatches <- makeMatch fParticipants
    case mMatches of
      Nothing      -> undefined
      Just matches -> forM_ matches \(gifter, receiver) -> 
        let Participant { pName = gifterName, pEmail = gifterEmail } = gifter
            Participant { pName = receiverName } = receiver
        in sendEmail gifterEmail
          $  "Dear "
          <> unrefine gifterName
          <> ", you are Secret Santa for "
          <> unrefine receiverName
