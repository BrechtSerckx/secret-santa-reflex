{-# OPTIONS_GHC -Wno-orphans #-}
module Text.EmailAddress
  ( EmailAddress
  )
where

import qualified Data.Aeson                    as Aeson
import           Data.Refine
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import "email-validate" Text.Email.Validate (EmailAddress, emailAddress, toByteString)

instance Refine Text EmailAddress where
  refine =
    emailAddress . T.encodeUtf8
      |>? "Invalid email. Format: my-email-adress@my-provider"
  unrefine = T.decodeUtf8With T.lenientDecode . toByteString
deriving via Refinable Text EmailAddress instance Aeson.ToJSON EmailAddress
deriving via Refinable Text EmailAddress instance Aeson.FromJSON EmailAddress
