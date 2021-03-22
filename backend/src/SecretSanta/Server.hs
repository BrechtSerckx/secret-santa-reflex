{-# LANGUAGE UndecidableInstances #-}
module SecretSanta.Server
  ( secretSantaServer
  )
where


import           Polysemy
import           Polysemy.Input
import           Polysemy.Input.Env

import           Control.Lens
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
import qualified Servant.API                   as S
import qualified Servant.API.TypeLevel         as S
import qualified Servant.Foreign               as SF
import qualified Servant.Foreign.Internal      as SF
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
apiServer opts = createSecretSantaHandler :<|> staticServer opts

staticServer :: Opts -> SS.ServerT Raw (Sem r)
staticServer Opts {..} =
  SS.serveDirectoryWith $ (Static.defaultWebAppSettings oWebRoot)
    { Static.ssRedirectToIndex = True
    , Static.ssIndices         = pure . Static.unsafeToPiece $ "index.html"
    }

class IsStatusCode status where
  errorConstructor :: Proxy status -> SS.ServerError
toServantError
  :: forall status name
   . (IsStatusCode status, KnownSymbol name)
  => ServerError status name
  -> SS.ServerError
toServantError se = (errorConstructor $ Proxy @status)
  { SS.errBody    = Aeson.encode se
  , SS.errHeaders = [("Content-Type", "application/json")]
  }
instance IsStatusCode 500 where
  errorConstructor Proxy = SS.err500
instance IsStatusCode 400 where
  errorConstructor Proxy = SS.err400

instance
  ( SF.HasForeignType lang ftype as
  , SF.ReflectMethod method
  , S.Elem SF.JSON list
  )
  => SF.HasForeign (lang::k) ftype (S.UVerb method list as) where
  type Foreign ftype (S.UVerb method list as) = SF.Req ftype
  foreignFor lang Proxy Proxy req =
    req
      & (SF.reqFuncName . SF._FunctionName %~ (methodLC :))
      & (SF.reqMethod .~ method)
      & (SF.reqReturnType .~ Just retType)
   where
    retType  = SF.typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy as)
    method   = SF.reflectMethod (Proxy :: Proxy method)
    methodLC = T.toLower $ decodeUtf8 method
