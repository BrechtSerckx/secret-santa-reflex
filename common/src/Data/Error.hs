module Data.Error
  ( ExtError
  , errDescription
  , errExtendedDescription
  , errContext
  , errWhen
  , errMessage
  , mkError
  , throwErrorPure
  , GenericError(..)
  , mkGenericError
  ) where

import           Control.Exception              ( throw )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (.=)
                                                )
import qualified Data.Text                     as T

data ExtError = ExtError
  { errDescription         :: Text
  , errExtendedDescription :: Maybe Text
  }
  deriving (Eq, Show)


errContext :: ExtError -> Text -> ExtError
errContext e ctx = e { errExtendedDescription = Just $ "in context " <> ctx }

errWhen :: ExtError -> Text -> ExtError
errWhen e ctx = e { errExtendedDescription = Just $ "when " <> ctx }

errMessage :: ExtError -> Text
errMessage ExtError {..} =
  "Internal error: "
    <> errDescription
    <> maybe "" (" " <>) errExtendedDescription

mkError :: Text -> ExtError
mkError errDescription =
  ExtError { errDescription, errExtendedDescription = Nothing }

throwErrorPure :: Exception ExtError => ExtError -> a
throwErrorPure = throw

instance Exception ExtError where
  displayException = T.unpack . errMessage

instance Aeson.ToPairs ExtError where
  toPairs ExtError {..} =
    [ "description" .= errDescription
    , "extendedDescription" .= errExtendedDescription
    ]

instance Aeson.FromJSON ExtError where
  parseJSON = Aeson.withObject "ExtError" $ \o -> do
    errDescription         <- o .: "description"
    errExtendedDescription <- o .:? "extendedDescription"
    pure ExtError { .. }

newtype GenericError (code :: Symbol) = GenericError
  { unGenericError :: ExtError } deriving newtype (Eq, Show, Exception)

mkGenericError :: Text -> GenericError code
mkGenericError = GenericError . mkError

instance KnownSymbol code => Aeson.ToJSON (GenericError code) where
  toJSON (GenericError err) =
    Aeson.object $ ["code" .= symbolVal (Proxy @code)] <> Aeson.toPairs err

instance KnownSymbol code => Aeson.FromJSON (GenericError code) where
  parseJSON v = flip (Aeson.withObject "GenericError") v $ \o -> do
    code <- o .: "code"
    guard $ code == symbolVal (Proxy @code)
    err <- Aeson.parseJSON v
    pure $ GenericError err
