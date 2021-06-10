{-# OPTIONS_GHC -Wno-orphans #-}
module Text.EmailAddress
  ( module Export
  )
where

import           Data.Refine
import "emailaddress" Text.EmailAddress        as Export
                                                ( EmailAddress
                                                , emailAddressFromText
                                                )
import "emailaddress" Text.EmailAddress         ( toText )

instance Refine Text EmailAddress where
  refine =
    emailAddressFromText
      |>? "Invalid email. Format: my-email-adress@my-provider"
  unrefine = toText
