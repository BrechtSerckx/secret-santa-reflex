{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module SecretSanta.Data
  ( module Export
  , InfoTable
  , ParticipantTable
  ) where

import           Data.Refine
import           Data.Time                      ( )
import "common"  SecretSanta.Data              as Export

import           Database.Beam                  ( Beamable )
import           Database.Beam.Backend.SQL      ( HasSqlValueSyntax(..) )
import           Database.Beam.Migrate          ( HasDefaultSqlDataType(..) )
import           Database.Beam.Orphans          ( )
import           Database.Beam.T2               ( T2(..) )

deriving
  via Refined Double Price
  instance HasSqlValueSyntax be Double => HasSqlValueSyntax be Price
deriving
  via Refined Double Price
  instance HasDefaultSqlDataType be Double => HasDefaultSqlDataType be Price

deriving anyclass instance Beamable SecretSantaIdT

type InfoTable = T2 SecretSantaIdT InfoT
deriving anyclass instance Beamable InfoT

type ParticipantTable = T2 SecretSantaIdT ParticipantT
deriving anyclass instance Beamable ParticipantT
