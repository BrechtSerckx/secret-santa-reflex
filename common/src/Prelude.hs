module Prelude
  ( module Export
  , headMay
  ) where


import           Data.Default.Class            as Export
import           Data.String                   as Export
                                                ( IsString(..)
                                                , String
                                                )
import           Data.Tuple.Extra              as Export
                                         hiding ( (&&&)
                                                , first
                                                , second
                                                )
import           GHC.Exts                      as Export
                                                ( IsList(..) )
import           GHC.Generics                  as Export
                                                ( Generic )
import           GHC.TypeLits                  as Export
                                                ( KnownSymbol
                                                , Symbol
                                                , symbolVal
                                                )
import           Relude                        as Export
                                         hiding ( State
                                                , evalState
                                                , gets
                                                , id
                                                , modify
                                                , toList
                                                )

headMay :: [b] -> Maybe b
headMay = viaNonEmpty head
