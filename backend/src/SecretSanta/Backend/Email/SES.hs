module SecretSanta.Backend.Email.SES
  ( SES
  ) where

import           Network.Mail.Mime
import qualified Network.Mail.Mime.SES         as SES
import           Polysemy
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Input.Env
import           Polysemy.Operators
import           SecretSanta.Backend.Email.Class
import           SecretSanta.Effect.Email
import qualified "this" System.Envy            as Env
import "this"    System.Envy                    ( (.<)
                                                , (.<?)
                                                )

data SESSettings = SESSettings
  { sesAccessKeyId     :: ByteString
  , sesSecretAccessKey :: ByteString
  , sesSessionToken    :: Maybe ByteString
  , sesRegion          :: Text
  }

instance Env.FromEnv SESSettings where
  fromEnv mDef = do
    sesAccessKeyId     <- "SES_ACCESS_KEY_ID" .< sesAccessKeyId <$> mDef
    sesSecretAccessKey <- "SES_SECRET_ACCESS_KEY" .< sesSecretAccessKey <$> mDef
    sesSessionToken    <- "SES_SESSION_TOKEN" .<? sesSessionToken <$> mDef
    sesRegion          <- "SES_REGION" .< sesRegion <$> mDef
    pure SESSettings { .. }

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
