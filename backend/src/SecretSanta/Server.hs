module SecretSanta.Server
  ( secretSantaServer
  ) where

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
    . SO.provideOptions (Proxy @API)
    . SS.serve (Proxy @API)
    $ apiServer
 where
  corsPolicy =
    CORS.simpleCorsResourcePolicy { CORS.corsRequestHeaders = ["content-type"] }

apiServer :: SS.Server API
apiServer = createSecretSantaHandler

createSecretSantaHandler :: SS.Server CreateSecretSantaEP
createSecretSantaHandler f = do
  liftIO $ print f
  pure ()
