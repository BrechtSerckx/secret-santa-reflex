module SecretSanta.Effect.SecretSantaStore
  ( SecretSantaStore
  , runSecretSantaStoreAsState
  , runSecretSantaStorePurely
  , runSecretSantaStoreDB
  , writeSecretSanta
  ) where

import           GHC.Err                        ( error )
import           Prelude                 hiding ( State )

import           Polysemy
import           Polysemy.KVStore
import           Polysemy.Operators
import           Polysemy.State
import           Polysemy.Transaction.Beam

import "this"    Database.Beam

import           SecretSanta.DB
import           SecretSanta.Data

type SecretSantaStore = KVStore SecretSantaId SecretSanta
type SecretSantaMap = Map SecretSantaId SecretSanta

writeSecretSanta :: SecretSantaId -> SecretSanta -> SecretSantaStore -@> ()
writeSecretSanta = writeKV

runSecretSantaStoreAsState
  :: SecretSantaStore ': r @> a -> State SecretSantaMap ': r @> a
runSecretSantaStoreAsState = runKVStoreAsState

runSecretSantaStorePurely
  :: SecretSantaMap -> SecretSantaStore ': r @> a -> r @> (SecretSantaMap, a)
runSecretSantaStorePurely = runKVStorePurely

runSecretSantaStoreDB
  :: (BeamC be, MonadBeam be bm)
  => DatabaseSettings be SecretSantaDB
  -> SecretSantaStore ': r @> a
  -> BeamTransaction be bm -@ r @> a
runSecretSantaStoreDB db = interpret $ \case
  UpdateKV k  (Just v) -> beamTransact $ insertSecretSanta db k v
  UpdateKV _k Nothing  -> error "SecretSantaStoreDB: delete not implemented"
  LookupKV _k          -> error "SecretSantaStoreDB: lookup not implemented"

insertSecretSanta
  :: (BeamC be, MonadBeam be m)
  => DatabaseSettings be SecretSantaDB
  -> SecretSantaId
  -> SecretSanta
  -> m ()
insertSecretSanta db id (SecretSanta UnsafeSecretSanta {..}) = do
  runInsert $ insertInfo db id secretsantaInfo
  runInsert $ insertParticipants db id secretsantaParticipants

insertInfo
  :: BeamC be
  => DatabaseSettings be SecretSantaDB
  -> SecretSantaId
  -> Info
  -> SqlInsert be InfoTable
insertInfo db id val =
  insert (_secretsantaInfo db) . insertValues . pure $ T2 (id, val)

insertParticipants
  :: BeamC be
  => DatabaseSettings be SecretSantaDB
  -> SecretSantaId
  -> Participants
  -> SqlInsert be ParticipantTable
insertParticipants db id ps =
  insert (_secretsantaParticipants db) . insertValues . fmap T2 $ (id, ) <$> ps
