module SecretSanta.Effect.Store
  ( Stores
  , module Export
  ) where

import           SecretSanta.Effect.Store.SecretSanta
                                               as Export

type Stores = '[SecretSantaStore]
