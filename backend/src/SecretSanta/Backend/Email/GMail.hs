module SecretSanta.Backend.Email.GMail
  ( GMail
  ) where

import           Polysemy
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Input.Env
import           SecretSanta.Backend.Email.Class
import           SecretSanta.Effect.Email

data GMail

instance RunEmailBackend GMail where
  emailBackendName = "gmail"
  data EmailBackendConfig GMail = EmailGMailConfig GmailSettings
  runEmailBackend act = do
    EmailGMailConfig c <- input
    runInputConst c . runEmailGmail . raiseUnder $ act
  runEmailBackendConfig =
    runInputEnv . contramapInput EmailGMailConfig . raiseUnder
