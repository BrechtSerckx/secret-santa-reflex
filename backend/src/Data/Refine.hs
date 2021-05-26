{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Refine
  ( module Export
  ) where

import "common"  Data.Refine                   as Export
import           Database.Beam.Backend          ( FromBackendRow(..)
                                                , HasSqlValueSyntax(..)
                                                )
import           Database.Beam.Migrate

instance (HasSqlValueSyntax be from, Refine from to) => HasSqlValueSyntax be (Refined from to) where
  sqlValueSyntax = sqlValueSyntax . rdeconstruct @from @to . unrefine

instance (FromBackendRow be from, Refine from to) => FromBackendRow be (Refined from to) where
  fromBackendRow =
    UnsafeRefined . unsafeRefine "fromBackendRow" <$> fromBackendRow

instance
  (HasDefaultSqlDataType be from, Refine from to)
  => HasDefaultSqlDataType be (Refined from to) where
  defaultSqlDataType Proxy proxy embedded =
    defaultSqlDataType (Proxy @from) proxy embedded
