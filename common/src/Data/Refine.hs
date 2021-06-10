{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Refine
  ( Refine(..)
  , Refined(..)
  , (|>)
  , (|>?)
  , refineTextMaybe
  , refineTextReadMaybe
  , unsafeRefine
  , withEither
  )
where

import           Prelude                 hiding ( from
                                                , to
                                                )

import           Control.Monad.Fail             ( fail )
import qualified Data.Aeson                    as Aeson
import           Data.Coerce                    ( Coercible
                                                , coerce
                                                )
import           Data.Error
import qualified Data.Text                     as T
import           Data.Validate

class Refine from to | to -> from where

  refine :: from -> Validated to

  unrefine :: to -> from
  default unrefine :: Coercible to from => to -> from
  unrefine = coerce @to @from

(|>) :: Bool -> Text -> Validated ()
b |> err = if b then failure err else success ()
infixl 5 |>

(|>?) :: (from -> Maybe to) -> Text -> from -> Validated to
(|>?) validateMaybe err from = case validateMaybe from of
  Just to -> success to
  Nothing -> failure err
infixl 5 |>?

withEither :: (from -> Either Text to) -> from -> Validated to
withEither validateEither from = case validateEither from of
  Right to  -> success to
  Left  err -> failure err

newtype Refined from to = UnsafeRefined { unRefined :: to }
  deriving newtype (Eq, Show)

instance (Aeson.ToJSON from, Refine from to) => Aeson.ToJSON (Refined from to) where
  toJSON = Aeson.toJSON . unrefine @from @to . unRefined

instance (Aeson.FromJSON from, Refine from to) => Aeson.FromJSON (Refined from to) where
  parseJSON v = Aeson.parseJSON v >>= \a -> case refine @from @to a of
    Failure es -> fail . show $ es
    Success a' -> pure . UnsafeRefined $ a'

refineTextMaybe :: Refine Text to => Text -> Validated (Maybe to)
refineTextMaybe t | T.null t  = Success Nothing
                  | otherwise = Just <$> refine t

refineTextReadMaybe
  :: (Read from, Refine from to) => Text -> Validated (Maybe to)
refineTextReadMaybe t
  | T.null t  = Success Nothing
  | otherwise = fmap Just $ readValidation t `bindValidation` refine

unsafeRefine :: Refine from to => Text -> from -> to
unsafeRefine ctx from = case refine from of
  Success to -> to
  Failure errs ->
    let msg = mconcat $ "Errors while using unsafeRefine: " : errs
    in  throwInternalError $ serverError msg `errContext` ctx
