{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
module SecretSanta.Interpret
  ( HandlerEffects
  , interpretBase
  , BaseEffects
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.Operators

import           Data.Error

import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.SecretSantaStore
import           SecretSanta.Effect.Time
import           SecretSanta.Email
import           SecretSanta.Opts

import qualified Database.SQLite.Simple        as SQLite

type BaseEffects eb
  = '[ Input Sender
     , Input SQLite.Connection
     , GetTime
     , Email
     , Input (EmailBackendConfig eb)
     , Embed IO
     , Final IO
     ]
interpretBase
  :: forall eb
   . (RunEmailBackend eb)
  => Opts
  -> SQLite.Connection
  -> SEmailBackend eb
  -> forall a . (BaseEffects eb @> a -> IO a)
interpretBase Opts {..} c eb =
  runFinal
    . embedToFinal
    . runEmailBackendConfig eb
    . runEmailBackend eb
    . runGetTime
    . runInputConst c
    . runInputConst (Sender oEmailSender)

type HandlerEffects eb kv
  = '[GetTime , SecretSantaStore , Error InternalError , Embed IO , Final IO]
