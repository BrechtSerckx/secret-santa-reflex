{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Aeson
  ( ToPairs(..)
  , withObject'
  , module Export
  ) where

import "aeson"   Data.Aeson                    as Export
import "aeson"   Data.Aeson.Types              as Export
import           Data.String                    ( String )

class ToPairs a where
  toPairs :: a -> [Pair]

instance ToPairs () where
  toPairs _ = []

instance ToPairs a => ToPairs (Maybe a) where
  toPairs = maybe mempty toPairs

instance {-# OVERLAPPABLE #-} ToPairs a => ToJSON a where
  toJSON = object . toPairs

withObject' :: String -> Value -> (Object -> Parser a) -> Parser a
withObject' n = flip $ withObject n
