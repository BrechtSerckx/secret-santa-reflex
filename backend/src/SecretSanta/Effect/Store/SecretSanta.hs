module SecretSanta.Effect.Store.SecretSanta
  ( SecretSantaStore
  , CrudStore(..)
  , createCrud
  , readCrud
  , readAllCrud
  , updateCrud
  , deleteCrud
  , runCrudStoreAsState
  , KVStoreInit
  , runKVStore
  , runKVStoreInit
  ) where

import qualified Data.Map.Strict               as Map
import           Prelude                 hiding ( State
                                                , evalState
                                                , gets
                                                , modify
                                                )

import           Polysemy
import           Polysemy.Input
import           Polysemy.Operators
import           Polysemy.State
import           Polysemy.Transaction.Beam

import "this"    Database.Beam

import           SecretSanta.Backend.KVStore.Class
import           SecretSanta.Backend.KVStore.Database
import           SecretSanta.Backend.KVStore.State
import           SecretSanta.Data
import           SecretSanta.Database
import           SecretSanta.Database.Tables

type SecretSantaStore = CrudStore SecretSantaId SecretSanta
type SecretSantaMap = Map SecretSantaId SecretSanta

data CrudStore k v m a where
  CreateCrud ::k -> v -> CrudStore k v m ()
  ReadCrud ::k  -> CrudStore k v m (Maybe v)
  ReadAllCrud ::CrudStore k v m [v]
  UpdateCrud ::k -> v -> CrudStore k v m ()
  DeleteCrud ::k -> CrudStore k v m ()
makeSem ''CrudStore

runCrudStoreAsState
  :: Ord k => CrudStore k v ': r @> a -> State (Map k v) ': r @> a
runCrudStoreAsState = reinterpret $ \case
  CreateCrud k v -> modify $ Map.insert k v
  ReadCrud k     -> gets $ Map.lookup k
  ReadAllCrud    -> gets Map.elems
  UpdateCrud k v -> modify $ Map.insert k v
  DeleteCrud k   -> modify $ Map.delete k

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
  CreateCrud k v -> do
    db <- input
    beamTransact @be @bm $ insertSecretSanta db k v
  ReadAllCrud -> do
    db <- input
    beamTransact @be @bm $ getAllSecretSantas db
  _ -> error "SecretSantaStoreDB: delete not implemented"

insertSecretSanta
  :: (BeamC be, MonadBeam be m)
  => DatabaseSettings be SecretSantaDB
  -> SecretSantaId
  -> SecretSanta
  -> m ()
insertSecretSanta db id (SecretSanta UnsafeSecretSanta {..}) = do
  let Info {..} = secretsantaInfo
  runInsert . insert (_secretsantaInfo db) . insertValues $ pure InfoRow
    { iId = id
    , ..
    }
  runInsert
    .   insert (_secretsantaParticipants db)
    .   insertValues
    $   secretsantaParticipants
    <&> \Participant {..} -> ParticipantRow { pId = id, .. }

getAllSecretSantas
  :: (BeamC be, MonadBeam be m)
  => DatabaseSettings be SecretSantaDB
  -> m [SecretSanta]
getAllSecretSantas db = do
  is :: [InfoRow] <- runSelectReturningList . select . all_ $ _secretsantaInfo
    db
  ps :: [ParticipantRow] <-
    runSelectReturningList . select . all_ $ _secretsantaParticipants db
  pure $ is <&> \InfoRow {..} -> SecretSanta UnsafeSecretSanta
    { secretsantaInfo         = Info { .. }
    , secretsantaParticipants = mapMaybe
      (\ParticipantRow {..} ->
        if iId == pId then Just Participant { .. } else Nothing
      )
      ps
    }

instance RunKVStore KVStoreState SecretSantaStore where
  data KVStoreInit KVStoreState SecretSantaStore m a
    = SecretSantaStoreStateInit { unSecretSantaStoreStateInit :: State SecretSantaMap m a}
  runKVStore =
    subsume . rewrite SecretSantaStoreStateInit . runCrudStoreAsState
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
