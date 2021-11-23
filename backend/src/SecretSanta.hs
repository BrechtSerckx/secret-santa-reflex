module SecretSanta
  ( runSecretSanta
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Log

import           Data.Error
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Text.Pretty.Simple

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
    $ do
        cmd <- embed parseCmd
        logInfo $ "Command: "
        logInfo . TL.toStrict $ pShow cmd
        case cmd of
          Serve    serveOpts@ServeOpts {..} -> secretSantaServer serveOpts
          CreateDB CreateDBOpts {..}        -> case cdbDatabaseBackend of
            AnyDatabaseBackend (Proxy :: Proxy db) opts -> do
              logInfo "Creating database ..."
              createDB @db opts
              logInfo "Done"
  logInfo "Have a nice day!"
  case res of
    Left e -> do
      logError $ errMessage e
      embed exitFailure
    Right () -> embed exitSuccess
