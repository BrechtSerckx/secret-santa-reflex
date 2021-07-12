{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Refine
  ( module Export
  ) where

import "common"  Data.Refine                   as Export
import           Database.Beam.Backend          ( FromBackendRow(..)
                                                , HasSqlValueSyntax(..)
                                                )
import           Database.Beam.Migrate          ( HasDefaultSqlDataType(..) )

instance (HasSqlValueSyntax be from, Refine from to) => HasSqlValueSyntax be (Refinable from to) where
  sqlValueSyntax = sqlValueSyntax . unrefine @from @to . unRefinable

instance (FromBackendRow be from, Refine from to) => FromBackendRow be (Refinable from to) where
  fromBackendRow =
    UnsafeRefinable . unsafeRefine "fromBackendRow" <$> fromBackendRow

instance
  (HasDefaultSqlDataType be from, Refine from to)
  => HasDefaultSqlDataType be (Refinable from to) where
  defaultSqlDataType Proxy proxy embedded =
    defaultSqlDataType (Proxy @from) proxy embedded
