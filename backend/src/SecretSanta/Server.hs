module SecretSanta.Server
  ( secretSantaServer
  ) where

import           Polysemy
import           Polysemy.Error

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

runInHandler :: forall r a . r ~ '[SecretSanta] => Sem r a -> SS.Handler a
runInHandler act =
  let runEmail = runEmailPrint
      runMatch = runMatchDet
  in  do
        eRes <-
          liftIO . runM . runEmail . runMatch . runError . runSecretSanta $ act
        case eRes of
          Right res -> pure res
          Left  e   -> throwError SS.err500 { SS.errBody = show e }

apiServer :: Member SecretSanta r => SS.ServerT API (Sem r)
apiServer = createSecretSantaHandler

createSecretSantaHandler :: Member SecretSanta r => Form -> Sem r ()
createSecretSantaHandler = createSecretSanta
