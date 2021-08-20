{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
module SecretSanta.Interpret
  ( interpretBase
  , BaseEffects
  ) where

import           Polysemy
import           Polysemy.Input
import           Polysemy.Operators

import           SecretSanta.Backend.Email
import           SecretSanta.Backend.KVStore
import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.Time
import           SecretSanta.Opts

type BaseEffects eb kvb
  = '[ Input Sender
     , GetTime
     , Email
     , Input (EmailBackendConfig eb)
     , KVStoreInit kvb SecretSantaStore
     , Input (KVConnection kvb)
     , Input (KVConfig kvb)
     , Embed IO
     , Final IO
     ]
interpretBase
  :: forall eb kvb a
   . (RunEmailBackend eb, RunKVBackend kvb, RunKVStores kvb)
  => Opts
  -> KVOpts kvb
  -> BaseEffects eb kvb @> a
  -> IO a
interpretBase Opts {..} cfg act =
  runFinal
    . embedToFinal
    . runKVConfig @kvb cfg
    . runKVConnection @kvb
    . runKVStoreInit @kvb
    . runEmailBackendConfig @eb
    . runEmailBackend @eb
    . runGetTime
    . runInputConst (Sender oEmailSender)
    $ act
