{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Polysemy.Error
  ( module Export
  , Envelope
  , RunErrorsU(..)
  , runErrorsU
  , hasErrorsU
  ) where

import "polysemy" Polysemy.Error               as Export
import           Polysemy.Operators

import           Data.Type
import           Data.Union

type Envelope u a = Either (Union u) a

class RunErrorsU es u where
  runErrorsU'
    :: TMap Error es :++ r @> Envelope u a
    -> r @> Envelope u a

instance RunErrorsU '[] u where
  runErrorsU' :: TMap Error '[] :++ r @> Envelope u a -> r @> Envelope u a
  runErrorsU' = identity

instance
  ( RunErrorsU es u
  , IsMember e u
  ) => RunErrorsU ((e :: *) ': es) u where
  runErrorsU'
    :: forall r a
     . TMap Error (e ': es) :++ r @> Envelope u a
    -> r @> Envelope u a
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
  => TMap Error es :++ r @> a
  -> r @> Envelope u a
runErrorsU = runErrorsU' @es @u . fmap Right

hasErrorsU :: forall es a . Envelope es a -> Bool
hasErrorsU = isLeft
