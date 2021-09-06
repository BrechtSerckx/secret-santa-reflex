module SecretSanta.Effect.Email
  ( Email(..)
  , sendEmail
  ) where

import           Network.Mail.Mime
import           Polysemy


data Email m a where
  -- | Send an email
  SendEmail ::Mail -> Email m ()

makeSem ''Email
