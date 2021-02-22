{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module SecretSanta.API
  ( API
  , api
  , PingEP
  , CreateSecretSantaEP
  ) where

import           Servant.API

import           SecretSanta.Data

type API = PingEP :<|> CreateSecretSantaEP

api = Proxy @API

type PingEP = "api" :> "ping" :> ReqBody '[JSON] () :> Post '[JSON] ()
type CreateSecretSantaEP
  = "api" :> "secret-santa" :> ReqBody '[JSON] Form :> Post '[JSON] ()
