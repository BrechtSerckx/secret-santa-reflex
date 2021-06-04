module SecretSanta.Effect.SecretSantaStore
  ( SecretSantaStore
  , runSecretSantaStoreAsState
  , runSecretSantaStorePurely
  , writeSecretSanta
  )
where

import           Prelude                 hiding ( State )

import           Polysemy.State
import           Polysemy.KVStore
import           Polysemy.Operators

import "common"  SecretSanta.Data

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
