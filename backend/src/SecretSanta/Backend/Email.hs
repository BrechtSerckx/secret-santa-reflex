module SecretSanta.Backend.Email
  ( EmailBackend(..)
  , SEmailBackend(..)
  , AnyEmailBackend(..)
  , RunEmailBackend(..)
  ) where

import           Polysemy
import           Polysemy.Input
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
