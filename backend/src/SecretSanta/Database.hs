module SecretSanta.Database
  ( SecretSantaDB(..)
  , BeamC
  ) where


import "this"    Database.Beam
import           SecretSanta.Data

-- * Database

data SecretSantaDB f = SecretSantaDB
  { _secretsantaInfo         :: f (TableEntity InfoTable)
  , _secretsantaParticipants :: f (TableEntity ParticipantTable)
  }
  deriving Generic
deriving anyclass instance Database be SecretSantaDB

-- brittany-disable-next-binding
type BeamC be
  = ( BeamSqlBackend be
    , FieldsFulfillConstraint
        (BeamSqlBackendCanSerialize be)
        InfoTable
    , FieldsFulfillConstraint
        (BeamSqlBackendCanSerialize be)
        ParticipantTable
    )
