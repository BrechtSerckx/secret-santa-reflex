module SecretSanta
  ( runSecretSanta
  ) where

import           Polysemy

import           SecretSanta.Backend.Email
import           SecretSanta.Backend.KVStore
import           SecretSanta.Interpret
import           SecretSanta.Opts
import           SecretSanta.Server

runSecretSanta :: IO ()
runSecretSanta = parseCmd >>= \case
  Serve serveOpts@ServeOpts {..} -> case (soEmailBackend, soKVStoreBackend) of
    (AnyEmailBackend (Proxy :: Proxy eb), AnyKVStoreBackend (cfg :: KVStoreOpts
        kvb))
      -> interpretBase @eb @kvb serveOpts cfg
        $ secretSantaServer @eb @kvb serveOpts
  CreateDB CreateDBOpts {..} -> case cdbDatabaseBackend of
    AnyDatabaseBackend (Proxy :: Proxy db) opts ->
      runM . runDBConfig @db opts $ createDB @db

