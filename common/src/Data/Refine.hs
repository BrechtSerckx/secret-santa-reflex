{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Refine
  ( Refine(..)
  , Refined(..)
  , (|>)
  , refineTextMaybe
  , refineTextReadMaybe
  , unsafeRefine
  ) where

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

  rguard :: from -> [Text]
  rguard = getFailures . refine @from @to

  rconstruct :: from -> to
  default rconstruct :: Coercible from to => from -> to
  rconstruct = coerce @from @to

  rdeconstruct :: to -> from
  default rdeconstruct :: Coercible to from => to -> from
  rdeconstruct = coerce @to @from

  refine :: from -> Validated to
  refine fa = case rguard @from @to fa of
    [] -> Success . rconstruct $ fa
    es -> Failure es

  {-# MINIMAL rguard | refine #-}

(|>) :: Bool -> Text -> [Text]
cond |> err = if cond then [err] else []
infixl 0 |>

newtype Refined from to = UnsafeRefined { unrefine :: to }
  deriving newtype (Eq, Show)

instance (Aeson.ToJSON from, Refine from to) => Aeson.ToJSON (Refined from to) where
  toJSON = Aeson.toJSON . rdeconstruct @from @to . unrefine

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
