{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module SecretSanta.Data
  ( module Export
  ) where

import           Data.Refine
import           Data.Time                      ( )
import "common"  SecretSanta.Data              as Export

import           Database.Beam                  ( Beamable )
import           Database.Beam.Backend.SQL      ( HasSqlValueSyntax(..) )
import           Database.Beam.Migrate          ( HasDefaultSqlDataType(..) )
import           Database.Beam.Orphans          ( )

deriving
  via Refined Double Price
  instance HasSqlValueSyntax be Double => HasSqlValueSyntax be Price
deriving
  via Refined Double Price
  instance HasDefaultSqlDataType be Double => HasDefaultSqlDataType be Price

deriving anyclass instance Beamable IntT
deriving anyclass instance Beamable InfoT
deriving anyclass instance Beamable ParticipantT
