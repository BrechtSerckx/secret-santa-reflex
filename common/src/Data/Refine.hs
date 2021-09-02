{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Refine
  ( Refine(..)
  , Refinable(..)
  , (|>)
  , (|>?)
  , refineTextMaybe
  , refineTextReadMaybe
  , unsafeRefine
  , withEither
  , RefineErrors(..)
  , Refined
  , renderRefineErrors
  , mappendMaybeErrors
  ) where

import           Control.Monad.Fail             ( fail )
import qualified Data.Aeson                    as Aeson
import           Data.Coerce                    ( Coercible
                                                , coerce
                                                )
import           Data.Error
import           Data.String                    ( IsString(..) )
import qualified Data.Text                     as T
import           Data.Validate

class Refine from to | to -> from where

  refine :: from -> Validation RefineErrors to

  unrefine :: to -> from
  default unrefine :: Coercible to from => to -> from
  unrefine = coerce @to @from

(|>) :: Bool -> RefineErrors -> Validation RefineErrors ()
b |> err = if b then Failure err else Success ()
infixl 5 |>

(|>?)
  :: (from -> Maybe to) -> RefineErrors -> from -> Validation RefineErrors to
(|>?) validateMaybe err from = case validateMaybe from of
  Just to -> Success to
  Nothing -> Failure err
infixl 5 |>?

withEither
  :: (from -> Either RefineErrors to) -> from -> Validation RefineErrors to
withEither validateEither from = case validateEither from of
  Right to  -> Success to
  Left  err -> Failure err

newtype Refinable from to = UnsafeRefinable { unRefinable :: to }
  deriving newtype (Eq, Show)

instance (Aeson.ToJSON from, Refine from to) => Aeson.ToJSON (Refinable from to) where
  toJSON = Aeson.toJSON . unrefine @from @to . unRefinable

instance (Aeson.FromJSON from, Refine from to) => Aeson.FromJSON (Refinable from to) where
  parseJSON v = Aeson.parseJSON v >>= \a -> case refine @from @to a of
    Failure es -> fail . show $ es
    Success a' -> pure . UnsafeRefinable $ a'

refineTextMaybe :: Refine Text to => Text -> Validation RefineErrors (Maybe to)
refineTextMaybe t | T.null t  = Success Nothing
                  | otherwise = Just <$> refine t

refineTextReadMaybe
  :: (Read from, Refine from to) => Text -> Validation RefineErrors (Maybe to)
refineTextReadMaybe t
  | T.null t  = Success Nothing
  | otherwise = fmap Just $ readValidation t `bindValidation` refine

unsafeRefine :: Refine from to => Text -> from -> to
unsafeRefine ctx from = case refine from of
  Success to -> to
  Failure errs ->
    let msg = T.pack $ displayException errs
    in  throwErrorPure $ mkError msg `errContext` ctx

newtype RefineErrors = RefineErrors { unRefineErrors :: NonEmpty Text }
  deriving newtype (Eq, Semigroup, Show)

instance IsString RefineErrors where
  fromString = RefineErrors . pure . T.pack

instance Exception RefineErrors where
  displayException (RefineErrors errs) =
    T.unpack $ "Refinement exception: " <> renderRefineErrors errs

type Refined = Validation RefineErrors

renderRefineErrors :: NonEmpty Text -> Text
renderRefineErrors = T.intercalate " + " . toList

mappendMaybeErrors
  :: Maybe RefineErrors -> Maybe RefineErrors -> Maybe RefineErrors
mappendMaybeErrors (Just e1) (Just e2) = Just $ e1 <> e2
mappendMaybeErrors (Just e ) Nothing   = Just e
mappendMaybeErrors Nothing   (Just e)  = Just e
mappendMaybeErrors Nothing   Nothing   = Nothing
