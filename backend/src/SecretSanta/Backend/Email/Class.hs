{-# LANGUAGE AllowAmbiguousTypes #-}
module SecretSanta.Backend.Email.Class
  ( AnyEmailBackend(..)
  , RunEmailBackend(..)
  ) where

import           Polysemy
import           Polysemy.Input
import           Polysemy.Operators

import qualified Data.Text                     as T
import           GHC.Show                       ( Show(..) )

import           SecretSanta.Effect.Email

data AnyEmailBackend where
  AnyEmailBackend ::RunEmailBackend eb => Proxy eb -> AnyEmailBackend

instance Show AnyEmailBackend where
  show = \case
    AnyEmailBackend (Proxy :: Proxy eb) ->
      T.unpack $ "AnyEmailBackend " <> emailBackendName @eb

class RunEmailBackend eb where
  emailBackendName :: Text
  data EmailBackendConfig eb :: *
  runEmailBackend
    :: Members '[Embed IO, Input (EmailBackendConfig eb) ] r
    => Email ': r @> a
    -> r @> a
  runEmailBackendConfig
    :: Member (Embed IO) r
    => Input (EmailBackendConfig eb) ': r @> a
    -> r @> a
