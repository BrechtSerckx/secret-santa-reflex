module SecretSanta.Server
  ( secretSantaServer
  ) where


import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.Input.Env

import           Control.Monad.Except           ( liftEither )
import qualified Data.ByteString.Lazy.Char8    as BSL
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
import qualified Servant.Server.StaticFiles    as SS
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

type HandlerEffects
  = '[Match, Email, GetTime , Input Sender, Error InvalidDateTimeError , Embed IO]

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
          . fmap join
          . (fmap (first toServantError) . try @SomeException)
          . fmap join
          . (fmap (first toServantError) . try @InternalError)
          . runM
          . (fmap (first toServantError) . runError)
          . runInputConst (Sender oEmailSender)
          . runGetTime
          . runEmail
          . runMatch
          $ act
        liftEither eRes

apiServer
  :: Opts -> SS.ServerT API' (Sem HandlerEffects)
apiServer opts = createSecretSantaHandler :<|> staticServer opts

staticServer :: Opts -> SS.ServerT Raw (Sem r)
staticServer Opts {..} =
  SS.serveDirectoryWith $ (Static.defaultWebAppSettings oWebRoot)
    { Static.ssRedirectToIndex = True
    , Static.ssIndices         = pure . Static.unsafeToPiece $ "index.html"
    }


-- TODO: remove, replace by UVerb
class ToServantError e where
  toServantError :: e -> SS.ServerError
instance ToServantError SomeException where
  toServantError e = SS.err500 { SS.errBody = BSL.pack $ displayException e }
class IsStatusCode status where
  errorConstructor :: Proxy status -> SS.ServerError
instance IsStatusCode status => ToServantError (ServerError status _name) where
  toServantError se = (errorConstructor $ Proxy @status)
    { SS.errBody = BSL.pack . T.unpack $ errMessage se
    }
instance IsStatusCode 500 where
  errorConstructor Proxy = SS.err500
instance IsStatusCode 400 where
  errorConstructor Proxy = SS.err400
