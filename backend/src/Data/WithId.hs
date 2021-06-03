{-# OPTIONS_GHC -Wno-orphans #-}
module Data.WithId
  ( module Export
  ) where

import "common"  Data.WithId                   as Export

import           Database.Beam                  ( Beamable
                                                , Table(..)
                                                )

deriving anyclass instance (Beamable idT, Beamable valT) => Beamable (WithIdT idT valT)

instance (Typeable idT, Beamable idT, Typeable valT, Beamable valT) => Table (WithIdT idT valT) where
  data PrimaryKey (WithIdT idT valT) f = SomeId (idT f)
    deriving (Generic, Beamable)
  primaryKey (WithId key _val) = SomeId key
