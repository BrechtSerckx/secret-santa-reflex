{-# OPTIONS_GHC -Wno-orphans #-}
module Text.EmailAddress
  ( module Export
  ) where

import           Data.Refine
import qualified Data.Text                     as T
import           Data.Validate
import "emailaddress" Text.EmailAddress        as Export
                                                ( EmailAddress
                                                , emailAddressFromText
                                                )
import "emailaddress" Text.EmailAddress         ( toText )

instance Refine Text EmailAddress where
  refine t | T.null t  = failure "Email cannot be empty"
           | otherwise = maybe invalidEmail success . emailAddressFromText $ t
   where
    invalidEmail = failure "Invalid email. Format: my-email-adress@my-provider"

  rdeconstruct = toText
  rconstruct   = undefined
