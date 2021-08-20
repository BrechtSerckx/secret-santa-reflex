module SecretSanta.Store
  ( Stores
  , module Export
  ) where

import           SecretSanta.Store.SecretSanta as Export

type Stores = '[SecretSantaStore]
