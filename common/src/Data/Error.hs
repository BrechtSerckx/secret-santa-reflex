{-# LANGUAGE KindSignatures #-}
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
  , throwInternalError
  ) where

import           Control.Exception              ( throw )
import qualified Data.Text                     as T

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

throwInternalError :: InternalError -> a
throwInternalError = throw

errStatus
  :: forall status _name . KnownNat status => ServerError status _name -> Int
errStatus _ = fromInteger . natVal $ Proxy @status
