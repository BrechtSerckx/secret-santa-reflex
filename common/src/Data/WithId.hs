module Data.WithId
  ( WithIdT(..)
  , WithId
  ) where

data WithIdT k t (f :: * -> *) = WithId
  { key   :: k f
  , value :: t f
  }
  deriving stock Generic
type WithId k t = WithIdT k t Identity
