module SecretSanta.Effect.Email
  ( Email
  , sendEmail
  , runEmailPrint
  , SESSettings(..)
  , runEmailSES
  , sesSettingsDecoder
  , GmailSettings(..)
  , runEmailGmail
  , gmailSettingsDecoder
  ) where

import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Text.Pretty.Simple

import           Polysemy
import           Polysemy.Input
import           Polysemy.Operators

import           Network.Mail.Mime
import qualified Network.Mail.Mime.SES         as SES
import qualified Network.Mail.SMTP             as SMTP

import qualified Environment                   as Env


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
  { sesAccessKey    :: ByteString
  , sesSecretKey    :: ByteString
  , sesSessionToken :: Maybe ByteString
  , sesRegion       :: Text
  }
runEmailSES :: Email ': r @> a -> IO ~@ Input SESSettings ': r @> a
runEmailSES = reinterpret $ \case
  SendEmail mail@Mail { mailFrom, mailTo } -> do
    SESSettings {..} <- input
    let ses = SES.SES { sesFrom = encodeUtf8 . addressEmail $ mailFrom
                      , sesTo   = encodeUtf8 . addressEmail <$> mailTo
                      , ..
                      }
    embed @IO . SES.renderSendMailSESGlobal ses $ mail

sesSettingsDecoder :: Env.Decoder SESSettings
sesSettingsDecoder = do
  sesAccessKey <- flip Env.variable bytes
    $ Env.Variable "SES_ACCESS_KEY" "SES access key" "12345"
  sesSecretKey <- flip Env.variable bytes
    $ Env.Variable "SES_SECRET_KEY" "SES secret key" "67890"
  sesSessionToken <- flip Env.variable mbytes
    $ Env.Variable "SES_SESSION_TOKEN" "SES session token" ""
  sesRegion <- flip Env.variable Env.text
    $ Env.Variable "SES_REGION" "SES region" "antarctica"
  pure SESSettings { .. }
 where
  bytes = T.encodeUtf8 <$> Env.text
  mbytes =
    Env.text <&> \t -> if T.null t then Nothing else Just . T.encodeUtf8 $ t

data GmailSettings = GmailSettings
  { gmailUsername :: Text
  , gmailPassword :: Text
  , gmailHost     :: Text
  , gmailPort     :: Int
  }
runEmailGmail :: Email ': r @> a -> IO ~@ Input GmailSettings ': r @> a
runEmailGmail = reinterpret $ \case
  SendEmail mail -> do
    GmailSettings {..} <- input
    let host     = T.unpack gmailHost
        port     = fromIntegral gmailPort
        username = T.unpack gmailUsername
        password = T.unpack gmailPassword
    embed @IO $ SMTP.sendMailWithLoginSTARTTLS' host port username password mail

gmailSettingsDecoder :: Env.Decoder GmailSettings
gmailSettingsDecoder = do
  gmailUsername <- flip Env.variable Env.text
    $ Env.Variable "GMAIL_USERNAME" "GMail username" "john.doe@gmail.com"
  gmailPassword <- flip Env.variable Env.text
    $ Env.Variable "GMAIL_PASSWORD" "GMail password" "i-like-unicorns123"
  gmailHost <- flip Env.variable Env.text
    $ Env.Variable "GMAIL_HOST" "GMail smtp host" "smtp.gmail.com"
  gmailPort <- flip Env.variable Env.int
    $ Env.Variable "GMAIL_PORT" "GMail smtp starttls port" "587"
  pure GmailSettings { .. }
