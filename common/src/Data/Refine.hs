{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Refine
  ( Refine(..)
  , Refined(..)
  , (|>)
  , refine
  , refineTextMaybe
  , refineTextReadMaybe
  ) where

import           Control.Monad.Fail             ( fail )
import qualified Data.Aeson                    as Aeson
import           Data.Coerce                    ( Coercible
                                                , coerce
                                                )
import qualified Data.Text                     as T
import           Data.Validate

class Refine from to | to -> from where

  rguard :: from -> [Text]

  construct :: from -> to
  default construct :: Coercible from to => from -> to
  construct = coerce @from @to

(|>) :: Bool -> Text -> [Text]
cond |> err = if cond then [err] else []
infixl 0 |>

refine :: forall from to . Refine from to => from -> Validated to
refine fa = case rguard @from @to fa of
  [] -> Success . construct $ fa
  es -> Failure es


newtype Refined from to = UnsafeRefined { unrefine :: to }
  deriving newtype (Eq, Show, Aeson.ToJSON)

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
