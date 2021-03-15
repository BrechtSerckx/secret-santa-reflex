{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module SecretSanta.API
  ( API
  , api
  , CreateSecretSantaEP
  ) where

import           Servant.API

import           SecretSanta.Data

type API = CreateSecretSantaEP

api = Proxy @API

type CreateSecretSantaEP
  = "api" :> "secret-santa" :> ReqBody '[JSON] Form :> Post '[JSON] ()
