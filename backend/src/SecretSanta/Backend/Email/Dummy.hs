module SecretSanta.Backend.Email.Dummy
  (Dummy) where

import           Polysemy.Input
import           SecretSanta.Backend.Email.Class
import           SecretSanta.Effect.Email

data Dummy

instance RunEmailBackend Dummy where
  emailBackendName = "dummy"
  data EmailBackendConfig Dummy = EmailDummyConfig
  runEmailBackend = runEmailPrint
  runEmailBackendConfig = runInputConst EmailDummyConfig
