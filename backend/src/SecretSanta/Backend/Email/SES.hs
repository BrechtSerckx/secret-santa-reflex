module SecretSanta.Backend.Email.SES
  () where

import           Polysemy
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Input.Env
import           SecretSanta.Backend.Email
import           SecretSanta.Effect.Email

instance RunEmailBackend 'SES where
  data EmailBackendConfig 'SES = EmailSESConfig SESSettings
  runEmailBackend SSES act = do
    EmailSESConfig c <- input
    runInputConst c . runEmailSES . raiseUnder $ act
  runEmailBackendConfig SSES =
    runInputEnv . contramapInput EmailSESConfig . raiseUnder
