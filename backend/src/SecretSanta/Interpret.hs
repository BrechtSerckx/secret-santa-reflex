module SecretSanta.Interpret
  ( HandlerEffects
  , interpretHandler
  ) where

import           Polysemy
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
     , Embed IO
     , Final IO
     ]

interpretHandler :: Opts -> HandlerEffects @> a -> IO (Either InternalError a)
interpretHandler Opts {..} act =
  let runEmail = case oEmailBackend of
        None  -> runEmailPrint
        GMail -> runInputEnv gmailSettingsDecoder . runEmailGmail
        SES   -> runInputEnv sesSettingsDecoder . runEmailSES
  in  liftIO . withConnection dbFile $ \conn -> do
        SQLite.setTrace conn (Just putStrLn)
        fmap join
          . fmap (first $ serverError . T.pack . displayException)
          . try @SomeException
          . try @InternalError
          . runFinal
          . embedToFinal
          . runInputConst (Sender oEmailSender)
          . runGetTime
          . runEmail
          . runInputConst conn
          $ act
