module SecretSanta.Backend.Email.Dummy
  () where

import           Polysemy.Input
import           SecretSanta.Backend.Email
import           SecretSanta.Effect.Email

instance RunEmailBackend 'None where
  data EmailBackendConfig 'None = EmailNoneConfig
  runEmailBackend SNone = runEmailPrint
  runEmailBackendConfig SNone = runInputConst EmailNoneConfig
