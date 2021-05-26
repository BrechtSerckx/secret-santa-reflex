module SecretSanta.Server
  ( secretSantaServer
  )
where


import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.Input.Env

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
import           Servant.API.UVerb
import qualified Servant.Server                as SS
import qualified WaiAppStatic.Types            as Static
                                                ( unsafeToPiece )

import           SecretSanta.API
import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.Match
import           SecretSanta.Effect.Time
import           SecretSanta.Handler.Create
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

type HandlerEffects = '[Match, Email, GetTime, Input Sender, Embed IO]

runInHandler :: forall a . Opts -> Sem HandlerEffects a -> SS.Handler a
runInHandler Opts {..} act =
  let runMatch = runMatchRandom
      runEmail = case oEmailBackend of
        None  -> runEmailPrint
        GMail -> runInputEnv gmailSettingsDecoder . runEmailGmail
        SES   -> runInputEnv sesSettingsDecoder . runEmailSES
  in  do
        eRes <-
          liftIO
          . fmap (first toServantError)
          . fmap join
          . fmap (first $ serverError . T.pack . displayException)
          . try @SomeException
          . try @InternalError
          . runM
          . runInputConst (Sender oEmailSender)
          . runGetTime
          . runEmail
          . runMatch
          $ act
        liftEither eRes

apiServer :: Opts -> SS.ServerT API' (Sem HandlerEffects)
apiServer opts =
  (runError . createSecretSantaHandler >=> \case
      Right r -> SS.respond $ WithStatus @200 r
      Left  e -> SS.respond e
    )
    :<|> staticServer opts

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
