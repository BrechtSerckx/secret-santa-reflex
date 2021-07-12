{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
module SecretSanta.Interpret
  ( HandlerEffects
  , interpretHandler
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
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
     , GetTime
     , Input Sender
     , Email
     , Error InternalError
     , Embed IO
     , Final IO
     ]

interpretHandler
  :: Opts
  -> '[ Input SQLite.Connection, GetTime, Input Sender, Email
      , Error InternalError, Embed IO
      , Final IO] @> a
  -> IO (Either InternalError a)
interpretHandler Opts {..} act =
  let aeb = AnyEmailBackend SNone
  in  liftIO $ case aeb of
        AnyEmailBackend (eb :: SEmailBackend eb) ->
          withConnection dbFile $ \conn -> do
            SQLite.setTrace conn (Just putStrLn)
            runFinal
              . embedToFinal
              . runError @InternalError
              . fromExceptionSem @InternalError
              . fromExceptionSemVia @SomeException
                  (internalError . T.pack . displayException)
              . runEmailBackendConfig eb
              . runEmailBackend eb
              . runInputConst (Sender oEmailSender)
              . runGetTime
              . runInputConst conn
              $ act
