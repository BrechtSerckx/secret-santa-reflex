module SecretSanta.Email
  ( EmailBackend(..)
  , SEmailBackend(..)
  , AnyEmailBackend(..)
  , RunEmailBackend(..)
  ) where

import           Polysemy
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Input.Env
import           Polysemy.Operators
import           SecretSanta.Effect.Email


data EmailBackend = None | GMail | SES
  deriving (Eq, Show, Read)

data SEmailBackend eb where
  SNone ::SEmailBackend 'None
  SGMail ::SEmailBackend 'GMail
  SSES ::SEmailBackend 'SES
data AnyEmailBackend where
  AnyEmailBackend ::RunEmailBackend eb => SEmailBackend eb ->AnyEmailBackend

class RunEmailBackend (eb:: EmailBackend) where
  data EmailBackendConfig eb :: *
  runEmailBackend
    :: Members '[Embed IO, Input (EmailBackendConfig eb) ] r
    => SEmailBackend eb
    -> Email ': r @> a
    -> r @> a
  runEmailBackendConfig
    :: Member (Embed IO) r
    => SEmailBackend eb
    -> Input (EmailBackendConfig eb) ': r @> a
    -> r @> a
instance RunEmailBackend 'None where
  data EmailBackendConfig 'None = EmailNoneConfig
  runEmailBackend SNone = runEmailPrint
  runEmailBackendConfig SNone = runInputConst EmailNoneConfig
instance RunEmailBackend 'GMail where
  data EmailBackendConfig 'GMail = EmailGMailConfig GmailSettings
  runEmailBackend SGMail act = do
    EmailGMailConfig c <- input
    runInputConst c . runEmailGmail . raiseUnder $ act
  runEmailBackendConfig SGMail =
    runInputEnv . contramapInput EmailGMailConfig . raiseUnder

instance RunEmailBackend 'SES where
  data EmailBackendConfig 'SES = EmailSESConfig SESSettings
  runEmailBackend SSES act = do
    EmailSESConfig c <- input
    runInputConst c . runEmailSES . raiseUnder $ act
  runEmailBackendConfig SSES =
    runInputEnv . contramapInput EmailSESConfig . raiseUnder
