{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Time
  ( module Export
  ) where

import "common"  Data.Time                     as Export

import           Database.Beam.Backend.SQL      ( HasSqlValueSyntax(..)
                                                , autoSqlValueSyntax
                                                )
import           Database.Beam.Migrate          ( HasDefaultSqlDataType(..) )
import           Database.Beam.Orphans          ( )

deriving newtype
  instance HasSqlValueSyntax be Day => HasSqlValueSyntax be Date

instance HasDefaultSqlDataType be Day => HasDefaultSqlDataType be Date where
  defaultSqlDataType Proxy proxy embedded =
    defaultSqlDataType (Proxy @Day) proxy embedded

instance HasSqlValueSyntax be [Char] => HasSqlValueSyntax be Time where
  sqlValueSyntax = autoSqlValueSyntax
instance HasDefaultSqlDataType be [Char] => HasDefaultSqlDataType be Time where
  defaultSqlDataType Proxy proxy embedded =
    defaultSqlDataType (Proxy @[Char]) proxy embedded

instance HasSqlValueSyntax be [Char] => HasSqlValueSyntax be TimeZone where
  sqlValueSyntax = autoSqlValueSyntax
instance HasDefaultSqlDataType be [Char] => HasDefaultSqlDataType be TimeZone where
  defaultSqlDataType Proxy proxy embedded =
    defaultSqlDataType (Proxy @[Char]) proxy embedded
