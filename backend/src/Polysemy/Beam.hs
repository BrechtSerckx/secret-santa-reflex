{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Polysemy.Beam
  ( -- * Utils
    Errors
  , (:++)
  , Connection(..)
  , Transaction(..)
  , transact
  , runTransaction'
  , runTransactionErrorsU
  , Envelope
  , RunErrorsU(..)
  , runErrorsU
  , hasErrorsU
  , BeamTransaction(..)
  , beamTransact
  , runBeamTransactionSqlite
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Operators

import           Data.SOP                       ( I(..) )

import           Database.Beam
import           Database.Beam.Sqlite           ( Sqlite
                                                , SqliteM
                                                , runBeamSqlite
                                                )
import qualified Database.SQLite.Simple        as SQLite

import           Servant.API.UVerb.Union        ( IsMember
                                                , Union
                                                , inject
                                                )

-- * Utils

type family Errors es where
  Errors '[] = '[]
  Errors (e ': es) = Error e ': Errors es

type family (:++) (as ::[k]) (bs ::[k]) :: [k] where
  '[] :++ bs = bs
  (a ': as) :++ bs = a ': (as :++ bs)
infixr 4 :++

-- * Connection class

class Connection c where
  startTransaction :: c -> IO ~@> ()
  endTransaction :: c -> IO ~@> ()
  rollbackTransaction :: c -> IO ~@> ()

instance Connection SQLite.Connection where
  startTransaction conn = embed $ SQLite.execute_ conn "BEGIN TRANSACTION"
  endTransaction conn = embed $ SQLite.execute_ conn "COMMIT TRANSACTION"
  rollbackTransaction conn =
    embed $ SQLite.execute_ conn "ROLLBACK TRANSACTION"

-- * Transactions

data Transaction c m a where
  Transact ::(c -> IO a) -> Transaction c m a
makeSem ''Transaction

runTransaction'
  :: Member (Embed IO) r => c -> Transaction c ': r @> a -> Input c ': r @> a
runTransaction' conn = reinterpret $ \case
  Transact f -> embed $ f conn

runTransactionErrorsU
  :: forall es a r c
   . ( Members '[Embed IO , Final IO] r
     , Members '[Embed IO , Final IO] (Errors es :++ r)
     , Connection c
     , RunErrorsU es es
     )
  => Transaction c ': (Errors es :++ r) @> a
  -> Input c ': r @> Envelope es a
runTransactionErrorsU act = do
  conn <- input
  startTransaction conn
  eRes <- runErrorsU @es . runTransaction' conn $ act
  if hasErrorsU @es eRes then rollbackTransaction conn else endTransaction conn
  pure eRes

type Envelope u a = Either (Union u) a

class RunErrorsU es u where
  runErrorsU'
    :: Input c ': Errors es :++ r @> Envelope u a
    -> Input c ': r @> Envelope u a
instance RunErrorsU '[] u where
  runErrorsU'
    :: Input c ': Errors '[] :++ r @> Envelope u a
    -> Input c ': r @> Envelope u a
  runErrorsU' = identity
instance
  ( RunErrorsU es u
  , IsMember e u
  ) => RunErrorsU ((e :: *) ': es) u where
  runErrorsU'
    :: forall r a c
     . Input c ': Errors (e ': es) :++ r @> Envelope u a
    -> Input c ': r @> Envelope u a
  runErrorsU' act = runErrorsU' @es $ do
    eEnv <- runError @e $ rotateEffects2 act
    let env' :: Envelope u a
        env' = case eEnv of
          Left  e         -> Left . inject $ I e
          Right (Left  e) -> Left e
          Right (Right a) -> Right a
    pure env'

runErrorsU
  :: forall es u c r a
   . RunErrorsU es u
  => Input c ': Errors es :++ r @> a
  -> Input c ': r @> Envelope u a
runErrorsU = runErrorsU' @es @u . fmap Right

hasErrorsU :: forall es a . Envelope es a -> Bool
hasErrorsU = isLeft

-- * Beam Transactions

data BeamTransaction be bm m a where
  BeamTransact ::(BeamSqlBackend be, MonadBeam be bm) => bm a -> BeamTransaction be bm m a
makeSem ''BeamTransaction

runBeamTransactionSqlite
  :: BeamTransaction Sqlite SqliteM ': r @> a
  -> Transaction SQLite.Connection ': r @> a
runBeamTransactionSqlite = reinterpret $ \case
  BeamTransact act -> transact $ \conn -> runBeamSqlite conn act
