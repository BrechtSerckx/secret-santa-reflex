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
  { iId          :: C f SecretSantaId
  , iEventName   :: C f EventName
  , iHostName    :: C f HostName
  , iHostEmail   :: C f HostEmail
  , iTimeZone    :: C f Time.TimeZone
  , iDate        :: C f (Maybe Time.Date)
  , iTime        :: C f (Maybe Time.Time)
  , iLocation    :: C f (Maybe Location)
  , iPrice       :: C f (Maybe Price)
  , iDescription :: C f Description
  }
  deriving stock Generic
  deriving anyclass Beamable
type InfoRow = InfoT Identity

instance Table InfoT where
  data PrimaryKey InfoT f = InfoId (C f SecretSantaId)
    deriving (Generic, Beamable)
  primaryKey = InfoId . iId


data ParticipantT f = ParticipantRow
  { pId    :: C f SecretSantaId
  , pName  :: C f PName
  , pEmail :: C f PEmail
  }
  deriving stock Generic
  deriving anyclass Beamable
type ParticipantRow = ParticipantT Identity


instance Table ParticipantT where
  data PrimaryKey ParticipantT f = ParticipantId (C f SecretSantaId) (C f PName)
    deriving (Generic, Beamable)
  primaryKey ParticipantRow {..} = ParticipantId pId pName
