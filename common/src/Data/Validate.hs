module Data.Validate
  ( module Export
  , getSuccess
  , getFailure
  , isSuccess
  , isFailure
  , allSuccesses
  , allFailures
  , validationToMaybe
  , readValidation
  , readValidationMaybe
  , bindValidation
  )
where

import           Data.Either.Validation        as Export
                                                ( Validation(..) )
import qualified Data.Text                     as T
import           Data.String                    ( IsString(..) )

getSuccess :: Validation e a -> Maybe a
getSuccess = \case
  Success a -> Just a
  Failure _ -> Nothing

getFailure :: Validation e a -> Maybe e
getFailure = \case
  Success _  -> Nothing
  Failure es -> Just es

isFailure :: Validation e a -> Bool
isFailure = isJust . getFailure
isSuccess :: Validation e a -> Bool
isSuccess = not . isFailure

allSuccesses :: [Validation e a] -> [a]
allSuccesses = mapMaybe getSuccess

validationToMaybe :: Validation e a -> Maybe a
validationToMaybe = getSuccess

allFailures :: [Validation e a] -> [e]
allFailures = mapMaybe getFailure

readValidation :: (Read a, IsString e) => Text -> Validation e a
readValidation t = case readMaybe . T.unpack $ t of
  Nothing -> Failure $ fromString "Cannot read value."
  Just a  -> Success a

readValidationMaybe :: (Read a, IsString e) => Text -> Validation e (Maybe a)
readValidationMaybe t
  | T.null t = Success Nothing
  | otherwise = fmap Just $ case readMaybe . T.unpack $ t of
    Nothing -> Failure "Cannot read value."
    Just a  -> Success a

bindValidation :: Validation e a -> (a -> Validation e b) -> Validation e b
bindValidation va f = case va of
  Failure e -> Failure e
  Success a -> f a
