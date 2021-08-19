{-# LANGUAGE AllowAmbiguousTypes #-}
module SecretSanta.Backend.Email
  ( module Export
  , readEmailBackends
  ) where

import SecretSanta.Backend.Email.Class as Export 
import SecretSanta.Backend.Email.Dummy as Export 
import SecretSanta.Backend.Email.GMail as Export 
import SecretSanta.Backend.Email.SES as Export 

type EmailBackends = '[Dummy, SES, GMail]

readEmailBackends :: Text -> Maybe AnyEmailBackend
readEmailBackends = readEmailBackends' @EmailBackends

class ReadEmailBackends ebs where
  readEmailBackends' :: Text -> Maybe AnyEmailBackend

instance ReadEmailBackends '[] where readEmailBackends' _ = Nothing
instance (RunEmailBackend eb, ReadEmailBackends ebs) => ReadEmailBackends (eb ': ebs) where
  readEmailBackends' t =
    if t == emailBackendName @eb
    then Just . AnyEmailBackend $ Proxy @eb
    else readEmailBackends' @ebs t
