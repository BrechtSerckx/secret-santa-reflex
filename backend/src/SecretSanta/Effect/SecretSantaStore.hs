module SecretSanta.Effect.SecretSantaStore
  ( SecretSantaStore
  , runSecretSantaStoreAsState
  , runSecretSantaStorePurely
  , runSecretSantaStoreDB
  , writeSecretSanta
  )
where

import           Prelude                 hiding ( State )
import GHC.Err (error)

import           Polysemy
import           Polysemy.State
import           Polysemy.KVStore
import           Polysemy.Operators

import "common"  SecretSanta.Data
import   SecretSanta.DB
import           Database.Beam

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
  :: (DecentBeamBackend be, MonadBeam be bm) => DatabaseSettings be SecretSantaDB -> SecretSantaStore ': r @> a -> Transaction bm -@ r @> a
runSecretSantaStoreDB db = interpret $ \case
  UpdateKV k (Just v) -> insertSecretSanta' db k v
  UpdateKV _k Nothing -> error "SecretSantaStoreDB: delete not implemented"
  LookupKV _k -> error "SecretSantaStoreDB: lookup not implemented"
