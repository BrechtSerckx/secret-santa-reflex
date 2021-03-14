module Data.Validate
  ( Validated
  , success
  , failure
  , failures
  , getFailures
  , hasFailures
  , isSuccess
  , readValidation
  , readValidationMaybe
  , bindValidation
  , module Export
  ) where

import           Data.Either.Validation        as Export
                                                ( Validation(..) )
import qualified Data.Text                     as T

type Validated a = Validation [Text] a

success :: a -> Validated a
success = Success
failure :: Text -> Validated a
failure = Failure . pure
failures :: [Text] -> Validated a
failures = Failure
getFailures :: Validated a -> [Text]
getFailures = \case
  Success _  -> []
  Failure es -> es
hasFailures :: Validated a -> Bool
hasFailures = null . getFailures
isSuccess :: Validated a -> Bool
isSuccess = not . hasFailures

readValidation :: Read a => Text -> Validated a
readValidation t = case readMaybe . T.unpack $ t of
  Nothing -> failure "Cannot read value."
  Just a  -> success a

readValidationMaybe :: Read a => Text -> Validated (Maybe a)
readValidationMaybe t
  | T.null t = success Nothing
  | otherwise = fmap Just $ case readMaybe . T.unpack $ t of
    Nothing -> failure "Cannot read value."
    Just a  -> success a

bindValidation :: Validated a -> (a -> Validated b) -> Validated b
bindValidation a f = case a of
  Failure es -> failures es
  Success a  -> f a
