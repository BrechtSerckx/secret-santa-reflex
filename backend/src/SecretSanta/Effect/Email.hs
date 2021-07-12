module SecretSanta.Effect.Email
  ( Email
  , sendEmail
  , runEmailPrint
  , SESSettings(..)
  , runEmailSES
  , GmailSettings(..)
  , runEmailGmail
  ) where

import qualified Data.Text                     as T
import           Text.Pretty.Simple

import           Polysemy
import           Polysemy.Input
import           Polysemy.Operators

import           Network.Mail.Mime
import qualified Network.Mail.Mime.SES         as SES
import qualified Network.Mail.SMTP             as SMTP

import qualified System.Envy                   as Env


data Email m a where
  -- | Send an email
  SendEmail ::Mail -> Email m ()

makeSem ''Email

runEmailPrint :: Email ': r @> a -> IO ~@ r @> a
runEmailPrint = interpret $ \case
  SendEmail mail -> embed @IO $ do
    putStrLn @Text @IO $ "Sending email:"
    pPrint mail

data SESSettings = SESSettings
  { sesAccessKeyId     :: ByteString
  , sesSecretAccessKey :: ByteString
  , sesSessionToken    :: Maybe ByteString
  , sesRegion          :: Text
  }
  deriving stock Generic
  deriving anyclass Env.FromEnv
runEmailSES :: Email ': r @> a -> IO ~@ Input SESSettings ': r @> a
runEmailSES = reinterpret $ \case
  SendEmail mail@Mail { mailFrom, mailTo } -> do
    SESSettings {..} <- input
    let ses = SES.SES { sesFrom      = encodeUtf8 . addressEmail $ mailFrom
                      , sesTo        = encodeUtf8 . addressEmail <$> mailTo
                      , sesAccessKey = sesAccessKeyId
                      , sesSecretKey = sesSecretAccessKey
                      , ..
                      }
    embed @IO . SES.renderSendMailSESGlobal ses $ mail

data GmailSettings = GmailSettings
  { gmailUsername :: Text
  , gmailPassword :: Text
  , gmailHost     :: Text
  , gmailPort     :: Int
  }
  deriving stock Generic
  deriving anyclass Env.FromEnv
runEmailGmail :: Email ': r @> a -> IO ~@ Input GmailSettings ': r @> a
runEmailGmail = reinterpret $ \case
  SendEmail mail -> do
    GmailSettings {..} <- input
    let host     = T.unpack gmailHost
        port     = fromIntegral gmailPort
        username = T.unpack gmailUsername
        password = T.unpack gmailPassword
    embed @IO $ SMTP.sendMailWithLoginSTARTTLS' host port username password mail
