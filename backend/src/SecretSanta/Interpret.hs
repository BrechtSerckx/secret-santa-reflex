{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
module SecretSanta.Interpret
  ( interpretBase
  , BaseEffects
  )
where

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
  = '[Input Sender, GetTime, Email, Input (EmailBackendConfig eb), KVStoreInit
    kvb
    SecretSantaStore, Input (KVStoreConnection kvb), Input (KVStoreConfig kvb), Embed
    IO, Final IO]
interpretBase
  :: forall eb kvb a
   . (RunEmailBackend eb, RunKVStoreBackend kvb, RunKVStores kvb)
  => ServeOpts
  -> KVStoreOpts kvb
  -> BaseEffects eb kvb @> a
  -> IO a
interpretBase ServeOpts {..} cfg act =
  runFinal
    . embedToFinal
    . runKVStoreConfig @kvb cfg
    . runKVStoreConnection @kvb
    . runKVStoreInit @kvb
    . runEmailBackendConfig @eb
    . runEmailBackend @eb
    . runGetTime
    . runInputConst (Sender soEmailSender)
    $ act
