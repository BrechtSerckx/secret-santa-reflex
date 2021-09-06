module SecretSanta.Server.Auth
  ( tokenAuthHandler
  , TokenAuthHandler
  ) where

import qualified Network.Wai                   as Wai
import           Servant.API                    ( AuthProtect )
import qualified Servant.Server.Experimental.Auth
                                               as SS

import           SecretSanta.API                ( TokenAuth )

type instance SS.AuthServerData (AuthProtect TokenAuth) = ()
type TokenAuthHandler
  = SS.AuthHandler Wai.Request (SS.AuthServerData (AuthProtect TokenAuth))

tokenAuthHandler :: SS.AuthHandler Wai.Request ()
tokenAuthHandler = SS.mkAuthHandler . const $ pure ()
