module SecretSanta.Backend.Email.GMail
  ( GMail
  ) where

import qualified Data.Text                     as T
import qualified Network.Mail.SMTP             as SMTP
import           Polysemy
import           Polysemy.Input
import           Polysemy.Input.Env
import           SecretSanta.Backend.Email.Class
import           SecretSanta.Effect.Email

import qualified System.Envy                   as Env

data GmailSettings = GmailSettings
  { gmailUsername :: Text
  , gmailPassword :: Text
  , gmailHost     :: Text
  , gmailPort     :: Int
  }
  deriving stock Generic
  deriving anyclass Env.FromEnv

data GMail

instance RunEmailBackend GMail where
  emailBackendName = "gmail"
  data EmailBackendConfig GMail = EmailGMailConfig GmailSettings
  runEmailBackend = interpret \case
    SendEmail mail -> do
      EmailGMailConfig (GmailSettings {..}) <- input
      let host     = T.unpack gmailHost
          port     = fromIntegral gmailPort
          username = T.unpack gmailUsername
          password = T.unpack gmailPassword
      embed @IO
        $ SMTP.sendMailWithLoginSTARTTLS' host port username password mail
  runEmailBackendConfig = interpret \case
    Input -> EmailGMailConfig <$> runInputEnv input
