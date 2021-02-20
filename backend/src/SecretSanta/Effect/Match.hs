module SecretSanta.Effect.Match
  ( Match
  , makeMatch
  , runMatchRandom
  , runMatchDet
  ) where

import           Polysemy

import qualified Data.List                     as List
import qualified System.Random.Shuffle         as Random

data Match m a where
  -- | Mat
  MakeMatch ::Eq a => [a] -> Match m (Maybe [(a,a)])
makeSem ''Match

runMatchRandom :: Sem (Match ': r) a -> Sem (Embed IO ': r) a
runMatchRandom = reinterpret $ \case
  MakeMatch xs -> embed $ match <$> Random.shuffleM xs

runMatchDet :: Sem (Match ': r) a -> Sem r a
runMatchDet = interpret $ \case
  MakeMatch xs -> pure $ match xs

match :: Eq a => [a] -> Maybe [(a, a)]
match as =
  let go []       [] = [[]]
      go []       _  = []
      go (x : xs) ys = do
        y <- ys
        guard $ x /= y
        map ((x, y) :) . go xs $ List.delete y ys
  in  headMay $ go as as
