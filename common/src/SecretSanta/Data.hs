{-# LANGUAGE DeriveAnyClass #-}
module SecretSanta.Data where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Time                      ( Day )

type Email = Text
-- type Participant = (Text, Email)
type Participant = Text
type Date = Text
type Price = Double


data Form = Form
  { fDescription  :: Text
  , fDate         :: Date
  , fPrice        :: Price
  , fParticipants :: [Participant]
  }
  deriving (Show, Generic, ToJSON, FromJSON)
