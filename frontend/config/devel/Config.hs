module Config
  ( baseUrl
  ) where

import qualified Servant.Reflex                as SR

baseUrl = SR.BaseFullUrl SR.Http "localhost" 8000 "/"
