module Database.Beam
  ( module Core
  , module Migrate
  , C'(..)
  ) where

import "beam-core" Database.Beam               as Core
import "beam-core" Database.Beam.Backend       as Core
                                                ( BeamSqlBackend
                                                , BeamSqlBackendCanSerialize
                                                )
import "beam-migrate" Database.Beam.Migrate    as Migrate
                                                ( CheckedDatabaseSettings
                                                , defaultMigratableDbSettings
                                                , unCheckDatabase
                                                )
import "beam-migrate" Database.Beam.Migrate.Simple
                                               as Migrate
                                                ( createSchema )
import "beam-core" Database.Beam.Schema.Tables as Core
                                                ( FieldsFulfillConstraint )

newtype C' a f = C' (C f a)
  deriving stock Generic
  deriving anyclass Beamable
