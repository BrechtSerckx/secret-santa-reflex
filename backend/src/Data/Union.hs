{-# LANGUAGE UndecidableInstances #-}
module Data.Union
  ( Union
  , UElem(..)
  , IsMember
  , module Export
  ) where

import           Data.SOP                      as Export
import           Data.Type

type Union = NS I

type IsMember (a :: u) (as :: [u])
  = (Unique as, CheckElemIsMember a as, UElem a as)

class UElem x xs where
  inject :: f x -> NS f xs
  eject :: NS f xs -> Maybe (f x)

instance {-# OVERLAPPING #-} UElem x (x ': xs) where
  inject = Z
  eject (Z x) = Just x
  eject _     = Nothing

instance {-# OVERLAPPING #-} UElem x xs => UElem x (x' ': xs) where
  inject = S . inject
  eject (Z _ ) = Nothing
  eject (S ns) = eject ns
