{-# LANGUAGE UndecidableInstances #-}
module Data.Type
  ( TMap
  , (:++)
  , Unique
  , Nubbed
  , Elem
  , CheckElemIsMember
  ) where

import           Data.Type.Bool                 ( If )
import           GHC.TypeLits
import           Prelude                 hiding ( TypeError )

type family TMap f es where
  TMap _ '[] = '[]
  TMap f (e ': es) = f e ': TMap f es

type family (:++) (as ::[k]) (bs ::[k]) :: [k] where
  '[] :++ bs = bs
  (a ': as) :++ bs = a ': (as :++ bs)
infixr 4 :++

-- brittany-disable-next-binding
type DuplicateElementError (rs :: [k]) =
          'Text "Duplicate element in list:"
    ':$$: 'Text "    " ':<>: 'ShowType rs

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem _ '[] = 'False
  Elem x (x' ': xs) =
    If (x == x') 'True (Elem x xs)

type family Unique xs :: Constraint where
  Unique xs = If (Nubbed xs == 'True) (() :: Constraint) (TypeError (DuplicateElementError xs))

type family Nubbed xs :: Bool where
  Nubbed '[] = 'True
  Nubbed (x ': xs) = If (Elem x xs) 'False (Nubbed xs)

-- | Check whether @a@ is in list.  This will throw nice errors if the element is not in the
-- list, or if there is a duplicate in the list.
type family CheckElemIsMember (a :: k) (as :: [k]) :: Constraint where
    CheckElemIsMember a as =
      If (Elem a as) (() :: Constraint) (TypeError (NoElementError a as))

-- brittany-disable-next-binding
type NoElementError (r :: k) (rs :: [k]) =
          'Text "Expected one of:"
    ':$$: 'Text "    " ':<>: 'ShowType rs
    ':$$: 'Text "But got:"
    ':$$: 'Text "    " ':<>: 'ShowType r
