{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module SecretSanta.API
  ( API
  , api
  , TokenAuth
  , SecretSantaAPI
  , CreateSecretSantaEP
  , InvalidDateTimeError
  ) where

import           Servant.API

import           Data.Error
import           Network.Http.Error
import           SecretSanta.Data

type API = SecretSantaAPI

type SecretSantaAPI = CreateSecretSantaEP :<|> GetSecretSantasEP

api :: Proxy API
api = Proxy @API

data TokenAuth

-- brittany-disable-next-binding
type CreateSecretSantaEP
  =  "api"
  :> "secret-santa"
  :> ReqBody '[JSON] SecretSantaCreate
  :> UVerb 'POST '[JSON]
     '[ WithStatus 200 SecretSantaId
      , InvalidDateTimeError
      ]
type InvalidDateTimeError = ApiError 400 (GenericError "INVALID_DATE_TIME")

-- brittany-disable-next-binding
type GetSecretSantasEP
  =  "api"
  :> "secret-santa"
  :> AuthProtect TokenAuth
  :> UVerb 'GET '[JSON]
     '[ WithStatus 200 [SecretSanta]
      ]
