module SecretSanta.Interpret
  ( HandlerEffects
  , interpretHandler
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.Input.Env
import           Polysemy.Operators

import           Data.Error
import qualified Data.Text                     as T
import qualified Database.SQLite.Simple        as SQLite

import           SecretSanta.DB
import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.Time
import           SecretSanta.Opts

type HandlerEffects
  = '[ Input SQLite.Connection
     , Email
     , GetTime
     , Input Sender
     , Error EnvError
     , Error InternalError
     , Embed IO
     , Final IO
     ]

interpretHandler :: Opts -> HandlerEffects @> a -> IO (Either InternalError a)
interpretHandler Opts {..} act =
  let
    runEmail :: Members '[Embed IO, Error EnvError] r => Email ': r @> a -> r @> a
    runEmail = case oEmailBackend of
        None  -> runEmailPrint
        GMail -> runInputEnv . runEmailGmail
        SES   -> runInputEnv . runEmailSES
  in  liftIO . withConnection dbFile $ \conn -> do
        SQLite.setTrace conn (Just putStrLn)
        runFinal
          . embedToFinal
          . runError @InternalError
          . fromExceptionSem @InternalError
          . fromExceptionSemVia  @SomeException (internalError . T.pack . displayException)
          . mapError (internalError . T.pack . unEnvError)
          . runInputConst (Sender oEmailSender)
          . runGetTime
          . runEmail
          . runInputConst conn
          $ act
