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
import           SecretSanta.Store

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
   . (RunEmailBackend eb, RunKVBackend kvb, FoldC (RunKVStore kvb) Stores)
  => Opts
  -> SKVBackend kvb
  -> KVConfig kvb
  -> BaseEffects eb kvb @> a
  -> IO a
interpretBase Opts {..} kvb cfg act =
  runFinal
    . embedToFinal
    . runKVConfig kvb cfg
    . runKVConnection kvb
    . runKVStoreInit @kvb kvb
    . runEmailBackendConfig @eb
    . runEmailBackend @eb
    . runGetTime
    . runInputConst (Sender oEmailSender)
    $ act
