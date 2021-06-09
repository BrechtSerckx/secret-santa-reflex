{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Database.Beam.Orphans
  () where

import           Data.Refine
import qualified "uuid" Data.UUID              as UUID
import           Data.Validate
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

instance Refine Text UUID.UUID where
  refine t = case UUID.fromText t of
    Just uuid -> success uuid
    Nothing   -> failure "UUID parser failed"
  rconstruct   = undefined
  rdeconstruct = UUID.toText

deriving
  via Refined Text UUID.UUID
  instance HasSqlValueSyntax be Text => HasSqlValueSyntax be UUID.UUID
deriving
  via Refined Text UUID.UUID
  instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be UUID.UUID

