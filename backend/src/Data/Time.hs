{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Time
  ( module Export
  ) where

import "common"  Data.Time                     as Export

import           Data.Refine
import           Database.Beam.Backend.SQL      ( FromBackendRow(..)
                                                , HasSqlValueSyntax(..)
                                                , autoSqlValueSyntax
                                                )
import           Database.Beam.Backend.Types    ( BeamBackend )
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

deriving
  via Refinable Text TimeZone
  instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be TimeZone

deriving
  via Refinable Text Date
  instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be Date

deriving
  via Refinable Text Time
  instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be Time

instance HasDefaultSqlDataType be [Char] => HasDefaultSqlDataType be UTCTime where
  defaultSqlDataType Proxy proxy embedded =
    defaultSqlDataType (Proxy @[Char]) proxy embedded
