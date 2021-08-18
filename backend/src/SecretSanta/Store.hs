module SecretSanta.Store
  ( Stores
  , module Export
  ) where

import           SecretSanta.Backend.KVStore
import           SecretSanta.Store.SecretSanta as Export

type Stores = '[SecretSantaStore]
