{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module SecretSanta.API
  ( API
  , api
  , CreateSecretSantaEP
  , InvalidDateTimeError
  ) where

import           Servant.API

import           Data.Error
import           SecretSanta.Data

type API = CreateSecretSantaEP

api :: Proxy API
api = Proxy @API

-- brittany-disable-next-binding
type CreateSecretSantaEP
  =  "api"
  :> "secret-santa"
  :> ReqBody '[JSON] SecretSanta
  :> UVerb 'POST '[JSON]
     '[ WithStatus 200 ()
      , InvalidDateTimeError
      ]
type InvalidDateTimeError = ServerError 400 "INVALID_DATE_TIME"
