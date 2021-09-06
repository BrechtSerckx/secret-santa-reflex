module SecretSanta.Server.Auth
  ( tokenAuthHandler
  , TokenAuthHandler
  ) where

import           Control.Monad.Except           ( throwError )
import qualified Data.List                     as L
import qualified Data.Text.Encoding            as T
import qualified Network.HTTP.Types            as Http
import qualified Network.Wai                   as Wai
import           Servant.API                    ( AuthProtect )
import qualified Servant.Server                as SS
import qualified Servant.Server.Experimental.Auth
                                               as SS

import           SecretSanta.API                ( TokenAuth
                                                , TokenAuthData
                                                )

type instance SS.AuthServerData (AuthProtect TokenAuth) = TokenAuthData
type TokenAuthHandler
  = SS.AuthHandler Wai.Request (SS.AuthServerData (AuthProtect TokenAuth))

tokenAuthHandler
  :: SS.AuthHandler Wai.Request (SS.AuthServerData (AuthProtect TokenAuth))
tokenAuthHandler = SS.mkAuthHandler $ \req ->
  case L.lookup Http.hAuthorization $ Wai.requestHeaders req of
    Nothing ->
      throwError $ SS.err401 { SS.errBody = "No 'Authorization' header" }
    Just x -> pure $ T.decodeUtf8 x
