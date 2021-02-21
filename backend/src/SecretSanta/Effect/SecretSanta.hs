module SecretSanta.Effect.SecretSanta
  ( SecretSanta
  , createSecretSanta
  , runSecretSantaPrint
  , runSecretSanta
  , InternalError(..)
  ) where

import           Polysemy
import           Polysemy.Error

import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.Match

data SecretSanta m a where
  -- | Create a new secret santa
  CreateSecretSanta ::Form -> SecretSanta m ()

data InternalError
  = NoMatchesFound [Participant] -- ^ No matches are found, so preconditions weren't met

makeSem ''SecretSanta

runSecretSantaPrint
  :: forall r a . Sem (SecretSanta ': r) a -> Sem (Embed IO ': r) a
runSecretSantaPrint = reinterpret $ \case
  CreateSecretSanta f -> embed $ print @IO f

runSecretSanta :: forall r a . Sem (SecretSanta ': r) a -> Sem (Error InternalError ': Match ': Email ': r) a
runSecretSanta = reinterpret3 $ \case
  CreateSecretSanta (Form UnsafeForm {..}) -> do
    mMatches <- makeMatch fParticipants
    case mMatches of
      Nothing      -> throw $ NoMatchesFound fParticipants
      Just matches -> forM_ matches \(gifter, receiver) -> 
        let Participant { pName = gifterName, pEmail = gifterEmail } = gifter
            Participant { pName = receiverName } = receiver
        in sendEmail gifterEmail
          $  "Dear "
          <> unNonEmptyText gifterName
          <> ", you are Secret Santa for "
          <> unNonEmptyText receiverName
