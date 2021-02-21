module SecretSanta.Effect.Email
  ( Email
  , sendEmail
  , runEmailPrint
  ) where

import           Polysemy

import           Text.EmailAddress

type EmailContents = Text

data Email m a where
  -- | Send an email
  SendEmail ::EmailAddress -> EmailContents -> Email m ()

makeSem ''Email

runEmailPrint :: Sem (Email ': r) a -> Sem (Embed IO ': r) a
runEmailPrint = reinterpret $ \case
  SendEmail dest contents ->
    embed
      .  print @IO
      $  "Sending to "
      <> emailAddressToText dest
      <> ": "
      <> contents
