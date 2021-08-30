module Polysemy.Log
  ( logMessageStdout
  , logDebug
  , logInfo
  , logWarning
  , logError
  , module Export
  ) where

import           Prelude                 hiding ( log )

import           Polysemy
import           Polysemy.Operators

import           Colog.Core
import           Colog.Message                 as Export
                                                ( Message
                                                , Msg(..)
                                                , fmtMessage
                                                )
import           Colog.Polysemy                as Export

import           Data.Functor.Contravariant
import qualified Data.Text                     as T

logMessageStdout :: MonadIO m => LogAction m Message
logMessageStdout = contramap (T.unpack . fmtMessage) logStringStdout

logMsg :: (Member (Log Message) r, HasCallStack) => Severity -> Text -> r @> ()
logMsg severity msg =
  log Msg { msgSeverity = severity, msgStack = callStack, msgText = msg }

logDebug :: (Member (Log Message) r, HasCallStack) => Text -> r @> ()
logDebug = logMsg Debug

logInfo :: (Member (Log Message) r, HasCallStack) => Text -> r @> ()
logInfo = logMsg Info

logWarning :: (Member (Log Message) r, HasCallStack) => Text -> r @> ()
logWarning = logMsg Warning

logError :: (Member (Log Message) r, HasCallStack) => Text -> r @> ()
logError = logMsg Error
