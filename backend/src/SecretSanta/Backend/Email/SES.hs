module SecretSanta.Backend.Email.SES
  ( SES
  ) where

import           Polysemy
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Input.Env
import           SecretSanta.Backend.Email.Class
import           SecretSanta.Effect.Email
import           Network.Mail.Mime
import qualified Network.Mail.Mime.SES         as SES
import qualified System.Envy                   as Env
import           Polysemy.Operators

data SESSettings = SESSettings
  { sesAccessKeyId     :: ByteString
  , sesSecretAccessKey :: ByteString
  , sesSessionToken    :: Maybe ByteString
  , sesRegion          :: Text
  }
  deriving stock Generic
  deriving anyclass Env.FromEnv

runEmailSES
  :: Members '[Embed IO , Input SESSettings] r => Email ': r @> a -> r @> a
runEmailSES = interpret $ \case
  SendEmail mail@Mail { mailFrom, mailTo } -> do
    SESSettings {..} <- input
    let ses = SES.SES { sesFrom      = encodeUtf8 . addressEmail $ mailFrom
                      , sesTo        = encodeUtf8 . addressEmail <$> mailTo
                      , sesAccessKey = sesAccessKeyId
                      , sesSecretKey = sesSecretAccessKey
                      , ..
                      }
    embed @IO . SES.renderSendMailSESGlobal ses $ mail

data SES

instance RunEmailBackend SES where
  emailBackendName = "ses"
  data EmailBackendConfig SES = EmailSESConfig SESSettings
  runEmailBackend act = do
    EmailSESConfig c <- input
    runInputConst c . runEmailSES . raiseUnder $ act
  runEmailBackendConfig =
    runInputEnv . contramapInput EmailSESConfig . raiseUnder
