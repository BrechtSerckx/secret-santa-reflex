module SecretSanta.Server.SecretSanta
  ( ssServer
  ) where

import           Network.Http.Error
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.Log
import           Polysemy.Operators
import           Servant.API                    ( (:<|>)(..) )
import           Servant.API.UVerb
import qualified Servant.Server                as SS

import           SecretSanta.API
import           SecretSanta.Backend.KVStore
import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.Time
import           SecretSanta.Server.Auth        ( )
import           SecretSanta.Server.SecretSanta.Create

ssServer
  :: forall kvb r
   . ( Members
         '[ Input Sender
          , Email
          , GetTime
          , KVStoreInit kvb SecretSantaStore
          , Embed IO
          , Input (KVStoreConnection kvb)
          , Log Message
          ]
         r
     , RunKVStores kvb
     )
  => SS.ServerT SecretSantaAPI (Sem (Error InternalError ': r))
ssServer = createSecretSantaHandler @kvb :<|> getSecretSantas @kvb

getSecretSantas
  :: forall kvb r
   . ( Members
         '[ Embed IO
          , Input (KVStoreConnection kvb)
          , KVStoreInit kvb SecretSantaStore
          ]
         r
     , RunKVStoreBackend kvb
     , RunKVStores kvb
     )
  => TokenAuthData
  -> r @> Union '[WithStatus 200 [SecretSanta]]
getSecretSantas token = do
  env :: Envelope '[] [SecretSanta] <-
    runKVStoreTransaction @kvb
    . fmap Right
    . runKVStore @kvb @SecretSantaStore
    . raiseUnder @(KVStoreTransaction kvb)
    $ readAllCrud
  SS.respond . WithStatus @200 $ fromEmptyEnvelope env

fromEmptyEnvelope :: Envelope '[] a -> a
fromEmptyEnvelope = \case
  Right a -> a
  Left  _ -> throwErrorPure $ mkError "Envelope without errors"
