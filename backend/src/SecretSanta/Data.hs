{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module SecretSanta.Data
  ( module Export
  , InfoTable
  , ParticipantTable
  )
where

import           Data.Refine
import           Data.Time                      ( )
import "common"  SecretSanta.Data              as Export

import           Database.Beam                  ( Beamable
                                                , Table(..)
                                                , C'(..)
                                                )
import           Database.Beam.Backend.SQL      ( HasSqlValueSyntax(..) )
import           Database.Beam.Migrate          ( HasDefaultSqlDataType(..) )
import           Database.Beam.Orphans          ( )
import           Database.Beam.T2               ( T2(..) )

deriving
  via Refinable Double Price
  instance HasSqlValueSyntax be Double => HasSqlValueSyntax be Price
deriving
  via Refinable Double Price
  instance HasDefaultSqlDataType be Double => HasDefaultSqlDataType be Price

deriving anyclass instance Beamable SecretSantaIdT
deriving anyclass instance Beamable InfoT
deriving anyclass instance Beamable ParticipantT

type InfoTable = T2 SecretSantaIdT InfoT
instance Table InfoTable where
  data PrimaryKey InfoTable f = InfoId (SecretSantaIdT f)
    deriving (Generic, Beamable)
  primaryKey (T2 (k, _)) = InfoId k


type ParticipantTable = T2 SecretSantaIdT ParticipantT
instance Table ParticipantTable where
  data PrimaryKey ParticipantTable f = ParticipantId (T2 SecretSantaIdT (C' PName) f)
    deriving (Generic, Beamable)
  primaryKey (T2 (k, Participant {..})) = ParticipantId $ T2 (k, C' pName)
