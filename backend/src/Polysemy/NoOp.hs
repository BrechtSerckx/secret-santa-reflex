module Polysemy.NoOp
  ( NoOp(..)
  , noOp
  , runNoOp
  ) where

import           Polysemy
import           Polysemy.Operators

data NoOp m a where
  NoOp ::a -> NoOp m a
makeSem ''NoOp

runNoOp :: NoOp ': r @> a -> r @> a
runNoOp = interpret \case
  NoOp a -> pure a
