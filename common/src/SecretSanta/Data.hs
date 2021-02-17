{-# LANGUAGE DeriveAnyClass #-}
module SecretSanta.Data where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Time                      ( Day )

type Email = Text
-- type Participant = (Text, Email)
data Participant = Participant
  { pName  :: Text
  , pEmail :: Email
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type Date = Day
type Price = Double


data Form = Form
  { fDescription  :: Text
  , fDate         :: Date
  , fPrice        :: Price
  , fParticipants :: [Participant]
  }
  deriving (Show, Generic, ToJSON, FromJSON)
