{-# LANGUAGE AllowAmbiguousTypes #-}
module SecretSanta.Server
  ( secretSantaServer
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.Log
import           Polysemy.Operators

import           Control.Monad.Except           ( liftEither )
import qualified "common" Data.Aeson           as Aeson
import qualified Data.Text                     as T
import           Network.Http.Error

import qualified Network.Wai.Application.Static
                                               as Static
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Middleware.Cors   as CORS
import qualified Network.Wai.Middleware.RequestLogger
                                               as RL
import qualified Network.Wai.Middleware.Servant.Options
                                               as SO
import           Servant.API                    ( (:<|>)(..)
                                                , Raw
                                                )
import qualified Servant.Server                as SS
import qualified WaiAppStatic.Types            as Static
                                                ( unsafeToPiece )

import           SecretSanta.API
import           SecretSanta.Backend.Email
import           SecretSanta.Backend.KVStore
import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.Time
import           SecretSanta.Opts
import           SecretSanta.Server.SecretSanta

type BaseEffects eb kvb
  = '[ Input Sender
     , GetTime
     , Email
     , Input (EmailBackendConfig eb)
     , KVStoreInit kvb SecretSantaStore
     , Input (KVStoreConnection kvb)
     , Input (KVStoreConfig kvb)
     , Log Message
     , Embed IO
     , Final IO
     ]

type API' = API :<|> Raw
api' :: Proxy API'
api' = Proxy @API'

secretSantaServer :: ServeOpts -> '[Log Message, Embed IO, Final IO] @> ()
secretSantaServer serveOpts@ServeOpts {..} = do
  logInfo "Starting server ..."
  case (soEmailBackend, soKVStoreBackend) of
    (AnyEmailBackend (Proxy :: Proxy eb), AnyKVStoreBackend (cfg :: KVStoreOpts
        kvb))
      -> secretSantaServer' @eb @kvb serveOpts cfg
  logInfo "Stopping server ..."

secretSantaServer'
  :: forall eb kvb
   . (RunEmailBackend eb, RunKVStore kvb SecretSantaStore)
  => ServeOpts
  -> KVStoreOpts kvb
  -> [Log Message, Embed IO, Final IO] @> ()
secretSantaServer' opts@ServeOpts {..} cfg =
  runKVStoreConfig @kvb cfg
    . runKVStoreConnection @kvb
    . runKVStoreInit @kvb
    . runEmailBackendConfig @eb
    . runEmailBackend @eb
    . runGetTime
    . runInputConst (Sender soEmailSender)
    $ do
        requestLogger <- embed $ RL.mkRequestLogger def
        withLowerToIO $ \lowerToIO finished -> do
          Warp.run soPort
            . requestLogger
            . CORS.cors (const $ Just corsPolicy)
            . SO.provideOptions api
            . SS.serve api'
            . SS.hoistServer api' (runInHandler lowerToIO)
            $ apiServer @eb @kvb opts
          finished
 where
  corsPolicy =
    CORS.simpleCorsResourcePolicy { CORS.corsRequestHeaders = ["content-type"] }


runInHandler
  :: Members '[Final IO , Log Message] r
  => (forall a . r @> a -> IO a)
  -> (forall a . Error InternalError ': r @> a -> SS.Handler a)
runInHandler lowerToIO act = do
  eRes <- liftIO . lowerToIO . fmap (first toServantError) $ do
    eRes <-
      runError @InternalError
      . fromExceptionSemVia @SomeException
          (internalError . T.pack . displayException)
      $ act
    case eRes of
      Right _                           -> pure ()
      Left  (ApiError (GenericError e)) -> logError $ errMessage e
    pure eRes
  liftEither eRes

apiServer
  :: forall eb kvb
   . RunKVStore kvb SecretSantaStore
  => ServeOpts
  -> SS.ServerT API' (Sem (Error InternalError ': BaseEffects eb kvb))
apiServer opts = ssServer @kvb :<|> staticServer opts

staticServer :: ServeOpts -> SS.ServerT Raw (Sem r)
staticServer ServeOpts {..} =
  SS.serveDirectoryWith $ (Static.defaultWebAppSettings soWebRoot)
    { Static.ssRedirectToIndex = True
    , Static.ssIndices         = pure . Static.unsafeToPiece $ "index.html"
    }

toServantError
  :: forall status a
   . (SS.IsStatusCode status, Aeson.ToJSON a)
  => ApiError status a
  -> SS.ServerError
toServantError se = (SS.errorConstructor $ Proxy @status)
  { SS.errBody    = Aeson.encode se
  , SS.errHeaders = [("Content-Type", "application/json")]
  }
