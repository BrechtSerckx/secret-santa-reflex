module Network.Http.Error
  ( ApiError(..)
  , errStatus
  , InternalError
  , internalError
  , module Export
  ) where

import qualified Data.Aeson                    as Aeson
import           Data.Error                    as Export
import qualified Servant.API                   as Servant
import qualified Servant.API.Status            as Servant

newtype ApiError (status :: Nat) a = ApiError
  { unApiError :: a
  }
  deriving newtype (Eq, Show, Aeson.ToJSON, Aeson.FromJSON)


errStatus :: forall status a . KnownNat status => ApiError status a -> Int
errStatus _ = fromIntegral . natVal $ Proxy @status

instance Servant.KnownStatus status => Servant.HasStatus (ApiError status a) where
  type StatusOf (ApiError status a) = status

type InternalError = ApiError 500 (GenericError "INTERNAL")

internalError :: Text -> InternalError
internalError = ApiError . GenericError . mkError
