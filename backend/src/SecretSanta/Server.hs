module SecretSanta.Server
  ( secretSantaServer
  ) where

import           Data.String

import           Polysemy
import           Polysemy.Error

import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Middleware.Cors   as CORS
import qualified Network.Wai.Middleware.RequestLogger
                                               as RL
import qualified Network.Wai.Middleware.Servant.Options
                                               as SO
import           Servant.API                    ( (:<|>)(..) )
import qualified Servant.Server                as SS

import           SecretSanta.API
import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.Match
import           SecretSanta.Effect.SecretSanta


secretSantaServer :: IO ()
secretSantaServer = do
  requestLogger <- RL.mkRequestLogger def
  Warp.run 8080
    . CORS.cors (const $ Just corsPolicy)
    . requestLogger
    . SO.provideOptions api
    . SS.serve api
    . SS.hoistServer api runInHandler
    $ apiServer
 where
  corsPolicy =
    CORS.simpleCorsResourcePolicy { CORS.corsRequestHeaders = ["content-type"] }

runInHandler
  :: forall r a
   . r ~ '[SecretSanta, Error InternalError, Embed IO]
  => Sem r a
  -> SS.Handler a
runInHandler act =
  let runMatch = runMatchDet
      runEmail = runEmailPrint
  in  do
        eRes <-
          liftIO . runM . runError . runEmail . runMatch . runSecretSanta $ act
        case eRes of
          Right res -> pure res
          Left  e   -> throwError SS.err500 { SS.errBody = show e }


apiServer :: Members '[SecretSanta , Embed IO] r => SS.ServerT API (Sem r)
apiServer = pingHandler :<|> createSecretSantaHandler

pingHandler :: () -> Sem r ()
pingHandler () = pure ()

createSecretSantaHandler
  :: Members '[SecretSanta , Embed IO] r => Form -> Sem r ()
createSecretSantaHandler f = do
  liftIO $ putStrLn @Text @IO "start"
  liftIO $ print f
  createSecretSanta f
  liftIO $ putStrLn @Text "done"
