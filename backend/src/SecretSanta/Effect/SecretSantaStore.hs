module SecretSanta.Effect.SecretSantaStore
  ( SecretSantaStore
  , runSecretSantaStoreAsState
  , runSecretSantaStorePurely
  , runSecretSantaStoreDB
  , writeSecretSanta
  )
where

import           Prelude                 hiding ( State )
import           GHC.Err                        ( error )

import           Polysemy
import           Polysemy.State
import           Polysemy.KVStore
import           Polysemy.Operators

import "this"    Database.Beam

import           SecretSanta.Data
import           SecretSanta.DB

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
  :: (DecentBeamBackend be, MonadBeam be bm)
  => DatabaseSettings be SecretSantaDB
  -> SecretSantaStore ': r @> a
  -> Transaction bm -@ r @> a
runSecretSantaStoreDB db = interpret $ \case
  UpdateKV k  (Just v) -> transact $ insertSecretSanta db k v
  UpdateKV _k Nothing  -> error "SecretSantaStoreDB: delete not implemented"
  LookupKV _k          -> error "SecretSantaStoreDB: lookup not implemented"

insertSecretSanta
  :: (DecentBeamBackend be, MonadBeam be m)
  => DatabaseSettings be SecretSantaDB
  -> SecretSantaId
  -> SecretSanta
  -> m ()
insertSecretSanta db id (SecretSanta UnsafeSecretSanta {..}) = do
  runInsert $ insertInfo db id secretsantaInfo
  runInsert $ insertParticipants db id secretsantaParticipants

insertInfo
  :: DecentBeamBackend be
  => DatabaseSettings be SecretSantaDB
  -> SecretSantaId
  -> Info
  -> SqlInsert be InfoTable
insertInfo db id val =
  insert (_secretsantaInfo db) . insertValues . pure $ T2 (id, val)

insertParticipants
  :: DecentBeamBackend be
  => DatabaseSettings be SecretSantaDB
  -> SecretSantaId
  -> Participants
  -> SqlInsert be ParticipantTable
insertParticipants db id ps =
  insert (_secretsantaParticipants db) . insertValues $ fmap T2 $ (id, ) <$> ps
