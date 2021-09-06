module SecretSanta.Server.Auth
  ( tokenAuthHandler
  , TokenAuthHandler
  ) where

import qualified Network.Wai                   as Wai
import           Servant.API                    ( AuthProtect )
import qualified Servant.Server.Experimental.Auth
                                               as SS

type instance SS.AuthServerData (AuthProtect "token") = ()
type TokenAuthHandler
  = SS.AuthHandler Wai.Request (SS.AuthServerData (AuthProtect "token"))

tokenAuthHandler :: SS.AuthHandler Wai.Request ()
tokenAuthHandler = SS.mkAuthHandler . const $ pure ()
