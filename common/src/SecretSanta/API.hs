{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module SecretSanta.API
  ( API
  , CreateSecretSantaEP
  ) where

import           Servant.API

import           SecretSanta.Data

type API = CreateSecretSantaEP

type CreateSecretSantaEP
  = "api" :> "secret-santa" :> ReqBody '[JSON] Form :> Post '[JSON] ()
