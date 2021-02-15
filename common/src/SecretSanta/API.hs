{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module SecretSanta.API
  ( API
  , EchoFormEP
  ) where

import           Servant.API

import           SecretSanta.Data

type API = EchoFormEP

type EchoFormEP
  = "api" :> "echo-form" :> ReqBody '[JSON] Form :> Post '[JSON] Form
