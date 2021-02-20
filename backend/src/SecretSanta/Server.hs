module SecretSanta.Server
  ( secretSantaServer
  ) where

import           Polysemy

import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Middleware.Cors   as CORS
import qualified Network.Wai.Middleware.Servant.Options
                                               as SO
import qualified Servant.Server                as SS

import           SecretSanta.API
import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.Match
import           SecretSanta.Effect.SecretSanta


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

runInHandler :: r ~ '[SecretSanta] => Sem r a -> SS.Handler a
runInHandler act =
  liftIO . runM . runEmailPrint . runMatchDet . runSecretSanta $ act

apiServer :: Member SecretSanta r => SS.ServerT API (Sem r)
apiServer = createSecretSantaHandler

createSecretSantaHandler :: Member SecretSanta r => Form -> Sem r ()
createSecretSantaHandler = createSecretSanta
