module SecretSanta.Server
  ( secretSantaServer
  ) where


import           Polysemy
import           Polysemy.Error
import           Polysemy.Input.Env

import           Control.Monad.Except           ( liftEither )
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Error

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
import qualified Servant.Server.StaticFiles    as SS
import qualified WaiAppStatic.Types            as Static
                                                ( unsafeToPiece )

import           SecretSanta.API
import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.Match
import           SecretSanta.Effect.SecretSanta
import           SecretSanta.Effect.Time
import           SecretSanta.Opts

type API' = API :<|> Raw
api' :: Proxy API'
api' = Proxy @API'

secretSantaServer :: IO ()
secretSantaServer = do
  opts@Opts { oPort } <- parseOpts
  requestLogger       <- RL.mkRequestLogger def
  Warp.run oPort
    . requestLogger
    . CORS.cors (const $ Just corsPolicy)
    . SO.provideOptions api
    . SS.serve api'
    . SS.hoistServer api' (runInHandler opts)
    $ apiServer opts
 where
  corsPolicy =
    CORS.simpleCorsResourcePolicy { CORS.corsRequestHeaders = ["content-type"] }

runInHandler
  :: forall r a
   . r ~ '[SecretSanta, GetTime, Error InvalidDateTimeError, Embed IO]
  => Opts
  -> Sem r a
  -> SS.Handler a
runInHandler Opts {..} act =
  let runMatch = runMatchRandom
      runEmail = case oEmailBackend of
        None  -> runEmailPrint
        GMail -> runInputEnv gmailSettingsDecoder . runEmailGmail
        SES   -> runInputEnv sesSettingsDecoder . runEmailSES
  in  do
        eRes <-
          liftIO
          . (fmap join . fmap (first fromException) . try) -- merge with exceptions
          . (fmap join . fmap (first fromInternalError) . try) -- merge with internal errors
          . fmap (first fromServerError) -- convert server error
          . runM
          . runError
          . runGetTime
          . runEmail
          . runMatch
          . runSecretSanta oEmailSender
          $ act
        liftEither eRes
 where
  fromInternalError :: InternalError -> SS.ServerError
  fromInternalError e =
    SS.err500 { SS.errBody = BSL.pack $ displayException e }
  fromException :: SomeException -> SS.ServerError
  fromException e = SS.err500 { SS.errBody = BSL.pack $ displayException e }
  fromServerError :: ServerError status name -> SS.ServerError
  fromServerError e =
    -- TODO
    SS.err400 { SS.errBody = BSL.pack $ show e }


apiServer
  :: Members '[SecretSanta , Embed IO] r => Opts -> SS.ServerT API' (Sem r)
apiServer opts = createSecretSantaHandler :<|> staticServer opts

createSecretSantaHandler
  :: Members '[SecretSanta , Embed IO] r => Form -> Sem r ()
createSecretSantaHandler f = do
  liftIO $ putStrLn @Text @IO "start"
  liftIO $ print f
  createSecretSanta f
  liftIO $ putStrLn @Text "done"

staticServer :: Opts -> SS.ServerT Raw (Sem r)
staticServer Opts {..} =
  SS.serveDirectoryWith $ (Static.defaultWebAppSettings oWebRoot)
    { Static.ssRedirectToIndex = True
    , Static.ssIndices         = pure . Static.unsafeToPiece $ "index.html"
    }
