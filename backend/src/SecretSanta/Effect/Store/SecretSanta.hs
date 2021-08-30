module SecretSanta.Effect.Store.SecretSanta
  ( SecretSantaStore
  , runSecretSantaStoreAsState
  , runSecretSantaStorePurely
  , runSecretSantaStoreDB
  , writeSecretSanta
  , KVStoreInit
  , runKVStore
  , runKVStoreInit
  ) where

import qualified Data.Map.Strict               as Map
import           GHC.Err                        ( error )
import           Prelude                 hiding ( State
                                                , evalState
                                                )

import           Polysemy
import           Polysemy.Input
import           Polysemy.KVStore
import           Polysemy.Operators
import           Polysemy.State
import           Polysemy.Transaction.Beam

import "this"    Database.Beam

import           SecretSanta.Backend.KVStore.Class
import           SecretSanta.Backend.KVStore.Database
import           SecretSanta.Backend.KVStore.State
import           SecretSanta.Data
import           SecretSanta.Database

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
  :: forall be bm r a
   . ( BeamC be
     , MonadBeam be bm
     , Members
         '[BeamTransaction be bm , Input (DatabaseSettings be SecretSantaDB)]
         r
     )
  => SecretSantaStore ': r @> a
  -> r @> a
runSecretSantaStoreDB = interpret $ \case
  UpdateKV k (Just v) -> do
    db <- input
    beamTransact @be @bm $ insertSecretSanta db k v
  UpdateKV _k Nothing -> error "SecretSantaStoreDB: delete not implemented"
  LookupKV _k         -> error "SecretSantaStoreDB: lookup not implemented"

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


instance RunKVStore KVStoreState SecretSantaStore where
  data KVStoreInit KVStoreState SecretSantaStore m a
    = SecretSantaStoreStateInit { unSecretSantaStoreStateInit :: State SecretSantaMap m a}
  runKVStore =
    subsume . rewrite SecretSantaStoreStateInit . runSecretSantaStoreAsState
  runKVStoreInit = evalState Map.empty . rewrite unSecretSantaStoreStateInit

instance IsDatabaseBackend db => RunKVStore (KVStoreDatabase db) SecretSantaStore where
  data KVStoreInit (KVStoreDatabase db) SecretSantaStore m a
    = SecretSantaStoreDatabaseInit
    { unSecretSantaStoreDatabaseInit :: Input (DatabaseSettings (BeamBackend db) SecretSantaDB) m a
    }
  runKVStore
    :: forall r a
     . Members
         '[ KVStoreTransaction (KVStoreDatabase db)
          , KVStoreInit (KVStoreDatabase db) SecretSantaStore
          ]
         r
    => SecretSantaStore ':r @> a
    -> r @> a
  runKVStore = runKVStore'
    SecretSantaStoreDatabaseInit
    unSecretSantaStoreDatabaseInit
    (runSecretSantaStoreDB @(BeamBackend db) @(BeamBackendM db))

  runKVStoreInit
    :: forall r a
     . KVStoreInit (KVStoreDatabase db) SecretSantaStore ': r @> a
    -> r @> a
  runKVStoreInit = runKVStoreInit' unSecretSantaStoreDatabaseInit
