{-# LANGUAGE DeriveAnyClass #-}
module SecretSanta.Data where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Time                      ( Day
                                                , TimeOfDay
                                                )

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
  { fName         :: Text
  , fDate         :: Maybe Date
  , fTime         :: Maybe TimeOfDay
  , fLocation     :: Maybe Text
  , fPrice        :: Maybe Price
  , fDescription  :: Text
  , fParticipants :: [Participant]
  }
  deriving (Show, Generic, ToJSON, FromJSON)
