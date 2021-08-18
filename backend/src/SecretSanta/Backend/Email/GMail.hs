module SecretSanta.Backend.Email.GMail
  () where

import           Polysemy
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Input.Env
import           SecretSanta.Backend.Email
import           SecretSanta.Effect.Email

instance RunEmailBackend 'GMail where
  data EmailBackendConfig 'GMail = EmailGMailConfig GmailSettings
  runEmailBackend SGMail act = do
    EmailGMailConfig c <- input
    runInputConst c . runEmailGmail . raiseUnder $ act
  runEmailBackendConfig SGMail =
    runInputEnv . contramapInput EmailGMailConfig . raiseUnder
