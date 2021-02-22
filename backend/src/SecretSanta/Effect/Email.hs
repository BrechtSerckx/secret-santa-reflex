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

import           Polysemy

import           Network.Mail.Mime
import qualified Network.Mail.Mime.SES         as SES
import qualified Network.Mail.SMTP             as SMTP
import           Text.EmailAddress

import           Text.Pretty.Simple

data Email m a where
  -- | Send an email
  SendEmail ::Mail -> Email m ()

makeSem ''Email

runEmailPrint :: Member (Embed IO) r => Sem (Email ': r) a -> Sem r a
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
runEmailSES
  :: Member (Embed IO) r => SESSettings -> Sem (Email ': r) a -> Sem r a
runEmailSES SESSettings {..} = interpret $ \case
  SendEmail mail@Mail { mailFrom, mailTo } ->
    let ses = SES.SES { sesFrom = encodeUtf8 . addressEmail $ mailFrom
                      , sesTo   = encodeUtf8 . addressEmail <$> mailTo
                      , ..
                      }
    in  embed @IO . SES.renderSendMailSESGlobal ses $ mail


data GmailSettings = GmailSettings
  { gmailUsername :: Text
  , gmailPassword :: Text
  }
runEmailGmail
  :: Member (Embed IO) r => GmailSettings -> Sem (Email ': r) a -> Sem r a
runEmailGmail GmailSettings {..} = interpret $ \case
  SendEmail mail ->
    let host     = "smtp.gmail.com"
        port_tls = 587
        -- port_ssl = 465
        port     = port_tls
        username = T.unpack gmailUsername
        password = T.unpack gmailPassword
    in  embed @IO
          $ SMTP.sendMailWithLoginSTARTTLS' host port username password mail

