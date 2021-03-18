module Config
  ( baseUrl
  ) where

import qualified Servant.Reflex                as SR

baseUrl = SR.BasePath "/"
