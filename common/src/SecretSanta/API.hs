{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module SecretSanta.API
  ( API
  , api
  , TokenAuth
  , TokenAuthData
  , AuthToken(..)
  , SecretSantaAPI
  , CreateSecretSantaEP
  , InvalidDateTimeError
  , AuthorizationError
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
type TokenAuthData = AuthToken
newtype AuthToken = AuthToken Text
  deriving newtype (Eq, Show, IsString)
type AuthorizationError = ApiError 401 (GenericError "AUTHORIZATION_FAILED")

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
      , AuthorizationError
      ]
