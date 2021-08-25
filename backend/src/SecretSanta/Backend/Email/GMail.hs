module SecretSanta.Backend.Email.GMail
  ( GMail
  ) where

import           Polysemy
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Input.Env
import           SecretSanta.Backend.Email.Class
import           SecretSanta.Effect.Email
import qualified Data.Text                     as T
import           Polysemy.Operators
import qualified Network.Mail.SMTP             as SMTP

import qualified System.Envy                   as Env

data GmailSettings = GmailSettings
  { gmailUsername :: Text
  , gmailPassword :: Text
  , gmailHost     :: Text
  , gmailPort     :: Int
  }
  deriving stock Generic
  deriving anyclass Env.FromEnv

runEmailGmail
  :: Members '[Embed IO , Input GmailSettings] r => Email ': r @> a -> r @> a
runEmailGmail = interpret $ \case
  SendEmail mail -> do
    GmailSettings {..} <- input
    let host     = T.unpack gmailHost
        port     = fromIntegral gmailPort
        username = T.unpack gmailUsername
        password = T.unpack gmailPassword
    embed @IO $ SMTP.sendMailWithLoginSTARTTLS' host port username password mail

data GMail

instance RunEmailBackend GMail where
  emailBackendName = "gmail"
  data EmailBackendConfig GMail = EmailGMailConfig GmailSettings
  runEmailBackend act = do
    EmailGMailConfig c <- input
    runInputConst c . runEmailGmail . raiseUnder $ act
  runEmailBackendConfig =
    runInputEnv . contramapInput EmailGMailConfig . raiseUnder
