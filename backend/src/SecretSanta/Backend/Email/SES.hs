module SecretSanta.Backend.Email.SES
  (SES) where

import           Polysemy
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Input.Env
import           SecretSanta.Backend.Email.Class
import           SecretSanta.Effect.Email

data SES

instance RunEmailBackend SES where
  emailBackendName = "ses"
  data EmailBackendConfig SES = EmailSESConfig SESSettings
  runEmailBackend act = do
    EmailSESConfig c <- input
    runInputConst c . runEmailSES . raiseUnder $ act
  runEmailBackendConfig =
    runInputEnv . contramapInput EmailSESConfig . raiseUnder
