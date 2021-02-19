module SecretSanta.Server
  ( secretSantaServer
  ) where

import           Polysemy

import           Data.Proxy
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Middleware.Cors   as CORS
import qualified Network.Wai.Middleware.Servant.Options
                                               as SO
import qualified Servant.Server                as SS

import           SecretSanta.API
import           SecretSanta.Data

secretSantaServer :: IO ()
secretSantaServer =
  Warp.run 8080
    . CORS.cors (const $ Just corsPolicy)
    . SO.provideOptions api
    . SS.serve api
    . SS.hoistServer api runInHandler
    $ apiServer
 where
  corsPolicy =
    CORS.simpleCorsResourcePolicy { CORS.corsRequestHeaders = ["content-type"] }

runInHandler :: r ~ '[Embed IO] => Sem r a -> SS.Handler a
runInHandler act = liftIO $ act & runM

apiServer :: Member (Embed IO) r => SS.ServerT API (Sem r)
apiServer = createSecretSantaHandler

createSecretSantaHandler :: Member (Embed IO) r => Form -> Sem r ()
createSecretSantaHandler f = do
  liftIO $ print f
  pure ()
