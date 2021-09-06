module SecretSanta.Server.SecretSanta
  ( ssServer
  , fromEmptyEnvelope
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Log
import           Polysemy.Operators

import           Data.SOP
import           Network.Http.Error

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
          , Input AuthToken
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
          , Input AuthToken
          ]
         r
     , RunKVStoreBackend kvb
     , RunKVStores kvb
     )
  => TokenAuthData
  -> r @> Union '[WithStatus 200 [SecretSanta], AuthorizationError]
getSecretSantas token = do
  env :: Envelope '[AuthorizationError] [SecretSanta] <-
    runKVStoreTransaction @kvb
    . runErrorsU @'[AuthorizationError]
    . rotateEffects2
    . runKVStore @kvb @SecretSantaStore
    . raiseUnder @(KVStoreTransaction kvb)
    $ do
        authorizeAdmin token
        readAllCrud
  case env of
    Left  e -> pure $ S e
    Right r -> SS.respond $ WithStatus @200 r

fromEmptyEnvelope :: Envelope '[] a -> a
fromEmptyEnvelope = \case
  Right a -> a
  Left  _ -> throwErrorPure $ mkError "Envelope without errors"

authorizeAdmin
  :: Members '[Input AuthToken , Error AuthorizationError] r
  => AuthToken
  -> r @> ()
authorizeAdmin token = do
  adminToken <- input @AuthToken
  if token == adminToken
    then pure ()
    else throw @AuthorizationError . ApiError $ mkGenericError
      "Authorization failed"
