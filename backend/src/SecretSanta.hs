module SecretSanta
  ( runSecretSanta
  ) where

import           Prelude                 hiding ( log )

import           Polysemy
import           Polysemy.Error

import           Colog.Core
import           Colog.Polysemy

import           Data.Error
import           Data.Functor.Contravariant
import qualified Data.Text                     as T

import           SecretSanta.Backend.KVStore
import           SecretSanta.Opts
import           SecretSanta.Server

runSecretSanta :: IO ()
runSecretSanta = runFinal . embedToFinal . runLogAction logTextStdout $ do
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
  case res of
    Left e -> do
      log $ errMessage e
      embed . die $ errMessage e
    Right () -> embed exitSuccess

logTextStdout :: MonadIO m => LogAction m Text
logTextStdout = contramap T.unpack logStringStdout
