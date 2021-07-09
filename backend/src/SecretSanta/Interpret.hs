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

interpretHandler :: Opts -> HandlerEffects @> a -> IO (Either InternalError a)
interpretHandler opts act =
  let aeb = AnyEmailBackend SNone
  in  liftIO $ case aeb of
        AnyEmailBackend (eb :: SEmailBackend eb) ->
          interpretHandler' opts eb act


interpretHandler'
  :: RunEmailBackend eb
  => Opts
  -> SEmailBackend eb
  -> HandlerEffects @> a
  -> IO (Either InternalError a)
interpretHandler' Opts {..} eb act = withConnection dbFile $ \conn -> do
  SQLite.setTrace conn (Just putStrLn)
  runFinal
    . embedToFinal
    . runError @InternalError
    . fromExceptionSem @InternalError
    . fromExceptionSemVia @SomeException
        (internalError . T.pack . displayException)
    . runEmailBackend eb
    . runInputConst (Sender oEmailSender)
    . runGetTime
    . runInputConst conn
    $ act
