{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Database.Beam.Orphans
  () where

import           Data.Refine
import "common"  Text.EmailAddress
import           Text.NonEmpty

import           Database.Beam.Backend.SQL      ( HasSqlValueSyntax(..) )
import           Database.Beam.Migrate          ( HasDefaultSqlDataType(..) )

instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be [Char] where
  defaultSqlDataType Proxy proxy embedded =
    defaultSqlDataType (Proxy @Text) proxy embedded

deriving
  via Refined Text NonEmptyText
  instance HasSqlValueSyntax be Text => HasSqlValueSyntax be NonEmptyText
deriving
  via Refined Text NonEmptyText
  instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be NonEmptyText
deriving
  via Refined Text EmailAddress
  instance HasSqlValueSyntax be Text => HasSqlValueSyntax be EmailAddress
deriving
  via Refined Text EmailAddress
  instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be EmailAddress

