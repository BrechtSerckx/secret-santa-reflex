{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Polysemy.Transaction
  ( -- * Utils
    Errors
  , (:++)
  , Connection(..)
  , Transaction(..)
  , transact
  , runTransaction'
  , runTransaction
  , Envelope
  , RunErrorsU(..)
  , runErrorsU
  , hasErrorsU
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Extra
import           Polysemy.Input
import           Polysemy.Operators

import           Data.SOP                       ( I(..) )
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
  :: Member (Embed IO) r => c -> Transaction c ': r @> a -> r @> a
runTransaction' conn = interpret $ \case
  Transact f -> embed $ f conn

runTransaction
  :: Members '[Embed IO , Input c] r => Transaction c ': r @> a -> r @> a
runTransaction act = do
  conn <- input
  runTransaction' conn act

type Envelope u a = Either (Union u) a

class RunErrorsU es u where
  runErrorsU'
    :: Errors es :++ r @> Envelope u a
    -> r @> Envelope u a
instance RunErrorsU '[] u where
  runErrorsU' :: Errors '[] :++ r @> Envelope u a -> r @> Envelope u a
  runErrorsU' = identity
instance
  ( RunErrorsU es u
  , IsMember e u
  ) => RunErrorsU ((e :: *) ': es) u where
  runErrorsU'
    :: forall r a . Errors (e ': es) :++ r @> Envelope u a -> r @> Envelope u a
  runErrorsU' act = runErrorsU' @es $ do
    eEnv <- runError @e act
    let env' :: Envelope u a
        env' = case eEnv of
          Left  e         -> Left . inject $ I e
          Right (Left  e) -> Left e
          Right (Right a) -> Right a
    pure env'

runErrorsU
  :: forall es u r a
   . RunErrorsU es u
  => Errors es :++ r @> a
  -> r @> Envelope u a
runErrorsU = runErrorsU' @es @u . fmap Right

hasErrorsU :: forall es a . Envelope es a -> Bool
hasErrorsU = isLeft
