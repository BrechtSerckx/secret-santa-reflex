module Prelude
  ( module Export
  , headMay
  ) where


import           Data.Default.Class            as Export
import           GHC.Generics                  as Export
                                                ( Generic )
import           GHC.TypeLits                  as Export
                                                ( KnownSymbol
                                                , Symbol
                                                , symbolVal
                                                )
import           Relude                        as Export
                                         hiding ( id )


import           Data.Tuple.Extra              as Export
                                         hiding ( (&&&)
                                                , first
                                                , second
                                                )

headMay :: [b] -> Maybe b
headMay = viaNonEmpty head
