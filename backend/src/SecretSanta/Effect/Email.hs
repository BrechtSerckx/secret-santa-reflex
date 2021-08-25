module SecretSanta.Effect.Email
  ( Email(..)
  , sendEmail
  ) where

import           Polysemy
import           Network.Mail.Mime


data Email m a where
  -- | Send an email
  SendEmail ::Mail -> Email m ()

makeSem ''Email
