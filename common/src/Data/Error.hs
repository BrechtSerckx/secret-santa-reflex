{-# LANGUAGE KindSignatures #-}
module Data.Error
  ( BError(..)
  , error
  , internalError
  ) where

data BError (status :: Nat) (name :: Symbol) = BError
  { errDescription :: Text
  , errContext     :: Maybe Text
  }
  deriving Show

error :: forall status name . Text -> BError status name
error errDescription = BError { errDescription, errContext = Nothing }

internalError :: Text -> a
internalError = undefined
