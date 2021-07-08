{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Error
  ( ServerError
  , errDescription
  , errExtendedDescription
  , errContext
  , errWhen
  , errMessage
  , errStatus
  , serverError
  , InternalError
  , internalError
  , throwInternalError
  ) where

import           Control.Exception              ( throw )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (.=)
                                                )
import qualified Data.Text                     as T
import qualified Servant.API                   as Servant
import qualified Servant.API.Status            as Servant

data ServerError (status :: Nat) (name :: Symbol) = ServerError
  { errDescription         :: Text
  , errExtendedDescription :: Maybe Text
  }
  deriving Show


errContext :: ServerError status name -> Text -> ServerError status name
errContext e ctx = e { errExtendedDescription = Just $ "in context " <> ctx }

errWhen :: ServerError status name -> Text -> ServerError status name
errWhen e ctx = e { errExtendedDescription = Just $ "when " <> ctx }

errMessage :: ServerError status name -> Text
errMessage ServerError {..} =
  "Internal error: "
    <> errDescription
    <> maybe "" (" " <>) errExtendedDescription

serverError :: forall status name . Text -> ServerError status name
serverError errDescription =
  ServerError { errDescription, errExtendedDescription = Nothing }


type InternalError = ServerError 500 "INTERNAL"

instance Exception InternalError where
  displayException = T.unpack . errMessage

internalError :: Text -> InternalError
internalError = serverError

throwInternalError :: InternalError -> a
throwInternalError = throw

errStatus
  :: forall status _name . KnownNat status => ServerError status _name -> Int
errStatus _ = fromInteger . natVal $ Proxy @status

instance Servant.KnownStatus status => Servant.HasStatus (ServerError status _name) where
  type StatusOf (ServerError status _name) = status

instance KnownSymbol name => Aeson.ToJSON (ServerError _status name) where
  toJSON ServerError {..} = Aeson.object
    [ "name" .= name
    , "description" .= errDescription
    , "extendedDescription" .= errExtendedDescription
    ]
    where name = symbolVal $ Proxy @name

instance KnownSymbol name => Aeson.FromJSON (ServerError status name) where
  parseJSON = Aeson.withObject ("ServerError-" <> name) $ \o -> do
    name' <- o .: "name"
    guard $ name == name'
    errDescription         <- o .: "description"
    errExtendedDescription <- o .:? "extendedDescription"
    pure ServerError { .. }
    where name = symbolVal $ Proxy @name
