module Type.Constraint
  ( FoldC
  ) where

type family FoldC mkC as :: Constraint where
  FoldC mkC '[] = ()
  FoldC mkC (a ': as) = (mkC a, FoldC mkC as)
