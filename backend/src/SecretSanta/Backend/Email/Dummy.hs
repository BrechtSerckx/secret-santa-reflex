module SecretSanta.Backend.Email.Dummy
  ( Dummy
  ) where

import           Polysemy
import           Polysemy.Input
import           Polysemy.Operators
import           SecretSanta.Backend.Email.Class
import           SecretSanta.Effect.Email
import           Text.Pretty.Simple

runEmailPrint :: Email ': r @> a -> IO ~@ r @> a
runEmailPrint = interpret $ \case
  SendEmail mail -> embed @IO $ do
    putStrLn @Text @IO $ "Sending email:"
    pPrint mail

data Dummy

instance RunEmailBackend Dummy where
  emailBackendName = "dummy"
  data EmailBackendConfig Dummy = EmailDummyConfig
  runEmailBackend       = runEmailPrint
  runEmailBackendConfig = runInputConst EmailDummyConfig
