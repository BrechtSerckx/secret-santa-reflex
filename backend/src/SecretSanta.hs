module SecretSanta
  ( runSecretSanta
  ) where

import           Prelude                 hiding ( log )

import           Polysemy
import           Polysemy.Error
import           Polysemy.Operators

import           Colog.Core
import           Colog.Message                  ( Message
                                                , Msg(..)
                                                , fmtMessage
                                                )
import           Colog.Polysemy

import           Data.Error
import           Data.Functor.Contravariant
import qualified Data.Text                     as T

import           SecretSanta.Backend.KVStore
import           SecretSanta.Opts
import           SecretSanta.Server

runSecretSanta :: IO ()
runSecretSanta = runFinal . embedToFinal . runLogAction logMessageStdout $ do
  logInfo "Welcome to Secret Santa"
  res <-
    runError @ExtError
    . fromExceptionSemVia @SomeException (mkError . T.pack . displayException)
    . raise @(Error ExtError)
    . raise
    $ do
        cmd <- embed parseCmd
        case cmd of
          Serve    serveOpts@ServeOpts {..} -> secretSantaServer serveOpts
          CreateDB CreateDBOpts {..}        -> case cdbDatabaseBackend of
            AnyDatabaseBackend (Proxy :: Proxy db) opts ->
              runDBConfig @db opts $ createDB @db
  logInfo "Have a nice day!"
  case res of
    Left e -> do
      logError $ errMessage e
      embed exitFailure
    Right () -> embed exitSuccess

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
