module SecretSanta.Database
  ( SecretSantaDB(..)
  , BeamC
  ) where


import "this"    Database.Beam
import           SecretSanta.Database.Tables

-- * Database

data SecretSantaDB f = SecretSantaDB
  { _secretsantaInfo         :: f (TableEntity InfoT)
  , _secretsantaParticipants :: f (TableEntity ParticipantT)
  }
  deriving Generic
deriving anyclass instance Database be SecretSantaDB

-- brittany-disable-next-binding
type BeamC be
  = ( BeamSqlBackend be
    , HasQBuilder be
    , FieldsFulfillConstraint
        (BeamSqlBackendCanSerialize be)
        InfoT
    , FieldsFulfillConstraint
        (BeamSqlBackendCanSerialize be)
        ParticipantT
    , FromBackendRow be (InfoT Identity)
    , FromBackendRow be (ParticipantT Identity)
    )
