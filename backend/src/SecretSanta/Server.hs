module SecretSanta.Server
  ( secretSantaServer
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Operators

import           Control.Monad.Except           ( liftEither )
import qualified Data.Aeson                    as Aeson
import           Data.Error
import qualified Data.Text                     as T

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
import           SecretSanta.Interpret
import           SecretSanta.Opts
import           SecretSanta.Server.SecretSanta
import           SecretSanta.Store.SecretSanta

type API' = API :<|> Raw
api' :: Proxy API'
api' = Proxy @API'

secretSantaServer :: IO ()
secretSantaServer = do
  opts@Opts {..} <- parseOpts
  case (oEmailBackend, oKVBackend) of
    (AnyEmailBackend eb, AnyKVBackendWithConfig kvb cfg) ->
      interpretBase opts eb kvb cfg $ secretSantaServer' opts eb kvb

secretSantaServer'
  :: forall eb kvb
   . (RunEmailBackend eb, RunKVStore kvb SecretSantaStore)
  => Opts
  -> SEmailBackend eb
  -> SKVBackend kvb
  -> BaseEffects eb kvb @> ()
secretSantaServer' opts@Opts {..} eb kvb = do
  requestLogger <- embed $ RL.mkRequestLogger def
  withLowerToIO $ \lowerToIO finished -> do
    Warp.run oPort
      . requestLogger
      . CORS.cors (const $ Just corsPolicy)
      . SO.provideOptions api
      . SS.serve api'
      . SS.hoistServer api' (runInHandler lowerToIO)
      $ apiServer eb kvb opts
    finished
 where
  corsPolicy =
    CORS.simpleCorsResourcePolicy { CORS.corsRequestHeaders = ["content-type"] }


runInHandler
  :: Member (Final IO) r
  => (forall a . r @> a -> IO a)
  -> (forall a . Error InternalError ': r @> a -> SS.Handler a)
runInHandler lowerToIO =
  -- liftEither <=< liftIO . lowerToIO . fmap Right
  liftEither
    <=< liftIO
    .   lowerToIO
    .   fmap (first toServantError)
    .   runError @InternalError
    .   fromExceptionSem @InternalError
    .   fromExceptionSemVia @SomeException
          (internalError . T.pack . displayException)

apiServer
  :: (RunEmailBackend eb, RunKVStore kvb SecretSantaStore)
  => SEmailBackend eb
  -> SKVBackend kvb
  -> Opts
  -> SS.ServerT
       API'
       (Sem (Error InternalError ': BaseEffects eb kvb))
apiServer eb kvb opts = createSecretSantaHandler eb kvb :<|> staticServer opts

staticServer :: Opts -> SS.ServerT Raw (Sem r)
staticServer Opts {..} =
  SS.serveDirectoryWith $ (Static.defaultWebAppSettings oWebRoot)
    { Static.ssRedirectToIndex = True
    , Static.ssIndices         = pure . Static.unsafeToPiece $ "index.html"
    }

toServantError
  :: forall status name
   . (SS.IsStatusCode status, KnownSymbol name)
  => ServerError status name
  -> SS.ServerError
toServantError se = (SS.errorConstructor $ Proxy @status)
  { SS.errBody    = Aeson.encode se
  , SS.errHeaders = [("Content-Type", "application/json")]
  }
