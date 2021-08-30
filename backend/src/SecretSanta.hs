module SecretSanta
  ( runSecretSanta
  ) where

import           Polysemy
import           Polysemy.Error

import           Data.Error
import qualified Data.Text                     as T

import           SecretSanta.Backend.KVStore
import           SecretSanta.Opts
import           SecretSanta.Server

runSecretSanta :: IO ()
runSecretSanta = runFinal . embedToFinal $ do
  res <-
    runError @ExtError
    . fromExceptionSemVia @SomeException (mkError . T.pack . displayException)
    . raise
    $ do
        cmd <- embed parseCmd
        case cmd of
          Serve    serveOpts@ServeOpts {..} -> secretSantaServer serveOpts
          CreateDB CreateDBOpts {..}        -> case cdbDatabaseBackend of
            AnyDatabaseBackend (Proxy :: Proxy db) opts ->
              runDBConfig @db opts $ createDB @db
  case res of
    Left  e  -> embed . die $ errMessage e
    Right () -> embed exitSuccess

