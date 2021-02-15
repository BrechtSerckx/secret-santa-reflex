{-# LANGUAGE DeriveAnyClass #-}
module SecretSanta.Data
  ( Form(..)
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )

data Form = Form
  { fName     :: Text
  , fLocation :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)
