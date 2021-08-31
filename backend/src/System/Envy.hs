{-# LANGUAGE UndecidableInstances #-}
module System.Envy
  ( module Export
  , parseEnv
  , (.<)
  , parseEnvMaybe
  , (.<?)
  ) where

import           Data.String                    ( String )
import "envy"    System.Envy                   as Export

parseEnv :: Var a => String -> Maybe a -> Parser a
parseEnv var = \case
  Just a  -> envMaybe var .!= a
  Nothing -> env var

(.<) :: Var a => String -> Maybe a -> Parser a
var .< mDef = parseEnv var mDef
infixr 3 .<

parseEnvMaybe :: Var a => String -> Maybe (Maybe a) -> Parser (Maybe a)
parseEnvMaybe var = \case
  Just a  -> (Just <$> envMaybe var) .!= a
  Nothing -> envMaybe var

(.<?) :: Var a => String -> Maybe (Maybe a) -> Parser (Maybe a)
var .<? mDef = parseEnvMaybe var mDef
infixr 3 .<?
