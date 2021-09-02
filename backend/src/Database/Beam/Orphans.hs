{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Database.Beam.Orphans
  () where

import           Data.Refine
import qualified "uuid" Data.UUID              as UUID
import "common"  SecretSanta.Data
import "common"  Text.EmailAddress
import           Text.NonEmpty

import           Database.Beam.Backend.SQL      ( FromBackendRow(..)
                                                , HasSqlValueSyntax(..)
                                                )
import           Database.Beam.Backend.Types    ( BeamBackend )
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
  via Refinable Text NonEmptyText
  instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be NonEmptyText

deriving
  via Refinable Text EmailAddress
  instance HasSqlValueSyntax be Text => HasSqlValueSyntax be EmailAddress
deriving
  via Refinable Text EmailAddress
  instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be EmailAddress
deriving
  via Refinable Text EmailAddress
  instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be EmailAddress

instance Refine Text UUID.UUID where
  refine   = UUID.fromText |>? "UUID parser failed"
  unrefine = UUID.toText

deriving
  via Refinable Text UUID.UUID
  instance HasSqlValueSyntax be Text => HasSqlValueSyntax be UUID.UUID
deriving
  via Refinable Text UUID.UUID
  instance HasDefaultSqlDataType be Text => HasDefaultSqlDataType be UUID.UUID
deriving
  via Refinable Text UUID.UUID
  instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be UUID.UUID

deriving
  via Refinable Double Price
  instance HasSqlValueSyntax be Double => HasSqlValueSyntax be Price
deriving
  via Refinable Double Price
  instance HasDefaultSqlDataType be Double => HasDefaultSqlDataType be Price
deriving
  via Refinable Double Price
  instance (BeamBackend be, FromBackendRow be Double) => FromBackendRow be Price

deriving
  via UUID.UUID
  instance HasSqlValueSyntax be UUID.UUID => HasSqlValueSyntax be SecretSantaId
deriving
  via UUID.UUID
  instance HasDefaultSqlDataType be UUID.UUID => HasDefaultSqlDataType be SecretSantaId
deriving
  via UUID.UUID
  instance (BeamBackend be, FromBackendRow be UUID.UUID) => FromBackendRow be SecretSantaId
