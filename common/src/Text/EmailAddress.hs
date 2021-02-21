module Text.EmailAddress
  ( module Export
  , validateEmailAddress
  , emailAddressToText
  ) where

import qualified Data.Text                     as T
import           Data.Validate
import "emailaddress" Text.EmailAddress        as Export
                                                ( EmailAddress
                                                , emailAddressFromText
                                                )
import "emailaddress" Text.EmailAddress         ( toText )

validateEmailAddress :: Text -> Validated EmailAddress
validateEmailAddress t
  | T.null t  = Failure . pure $ "Email cannot be empty"
  | otherwise = maybe invalidEmail Success . emailAddressFromText $ t
 where
  invalidEmail =
    Failure . pure $ "Invalid email. Format: my-email-adress@my-provider"

emailAddressToText :: EmailAddress -> Text
emailAddressToText = toText
