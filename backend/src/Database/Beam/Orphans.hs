{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Database.Beam.Orphans
  () where

import           Data.Refine
import qualified "uuid" Data.UUID              as UUID
import "common"  Text.EmailAddress
import           Text.NonEmpty

import           Database.Beam.Backend.SQL      ( HasSqlValueSyntax(..) )
import           Database.Beam.Migrate          ( HasDefaultSqlDataType(..) )

instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be [Char] where
  defaultSqlDataType Proxy proxy embedded =
    defaultSqlDataType (Proxy @Text) proxy embedded

deriving
  via Refinable Text NonEmptyText
  instance HasSqlValueSyntax be Text => HasSqlValueSyntax be NonEmptyText
deriving
  via Refinable Text NonEmptyText
  instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be NonEmptyText

deriving
  via Refinable Text EmailAddress
  instance HasSqlValueSyntax be Text => HasSqlValueSyntax be EmailAddress
deriving
  via Refinable Text EmailAddress
  instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be EmailAddress

instance Refine Text UUID.UUID where
  refine   = UUID.fromText |>? "UUID parser failed"
  unrefine = UUID.toText

deriving
  via Refinable Text UUID.UUID
  instance HasSqlValueSyntax be Text => HasSqlValueSyntax be UUID.UUID
deriving
  via Refinable Text UUID.UUID
  instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be UUID.UUID

