module SecretSanta.Database.Tables
  ( InfoT(..)
  , InfoRow
  , ParticipantT(..)
  , ParticipantRow
  ) where

import qualified Data.Time                     as Time
import           Database.Beam
import           Database.Beam.Orphans          ( )
import "common"  SecretSanta.Data

data InfoT f = InfoRow
  { iId         :: C f SecretSantaId
  , eventName   :: C f EventName
  , hostName    :: C f HostName
  , hostEmail   :: C f HostEmail
  , timeZone    :: C f Time.TimeZone
  , mDate       :: C f (Maybe Time.Date)
  , mTime       :: C f (Maybe Time.Time)
  , mLocation   :: C f (Maybe Location)
  , mPrice      :: C f (Maybe Price)
  , description :: C f Description
  }
  deriving stock Generic
  deriving anyclass Beamable
type InfoRow = InfoT Identity

instance Table InfoT where
  data PrimaryKey InfoT f = InfoId (C f SecretSantaId)
    deriving (Generic, Beamable)
  primaryKey = InfoId . iId


data ParticipantT f = ParticipantRow
  { pId   :: C f SecretSantaId
  , name  :: C f PName
  , email :: C f PEmail
  }
  deriving stock Generic
  deriving anyclass Beamable
type ParticipantRow = ParticipantT Identity


instance Table ParticipantT where
  data PrimaryKey ParticipantT f = ParticipantId (C f SecretSantaId) (C f PName)
    deriving (Generic, Beamable)
  primaryKey ParticipantRow {..} = ParticipantId pId name
