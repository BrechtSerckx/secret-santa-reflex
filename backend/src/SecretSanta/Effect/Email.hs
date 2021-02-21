module SecretSanta.Effect.Email
  ( Email
  , sendEmail
  , runEmailPrint
  ) where

import           Polysemy

import           Network.Mail.Mime
import           Text.EmailAddress

import           Text.Pretty.Simple

type EmailContents = Text

data Email m a where
  -- | Send an email
  SendEmail ::Mail -> Email m ()

makeSem ''Email

runEmailPrint :: Sem (Email ': r) a -> Sem (Embed IO ': r) a
runEmailPrint = reinterpret $ \case
  SendEmail mail -> embed @IO $ do
    putStrLn @Text @IO $ "Sending email:"
    pPrint mail

