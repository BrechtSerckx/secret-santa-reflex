{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module SecretSanta.Handler.Create
  ( createSecretSantaHandler
  , InvalidDateTimeError
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Input

import           Data.Error
import           Data.Time
import           Data.Time.MonadTime
import           Data.Validate
import           Text.NonEmpty

import           Network.Mail.Mime
import qualified Text.Blaze.Html.Renderer.Text as BlazeHtml
import qualified Text.Blaze.Renderer.Text      as BlazeText
import "common"  Text.EmailAddress
import           Text.Hamlet

import           SecretSanta.API
import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.Match
import           SecretSanta.Effect.Time        ( GetTime )


createSecretSantaHandler
  :: Members
       '[ Input Sender
        , GetTime
        , Match
        , Email
        , Embed IO
        , Error InvalidDateTimeError
        ]
       r
  => Form
  -> Sem r ()
createSecretSantaHandler f@(Form UnsafeForm {..}) = do
  sender     <- input @Sender
  serverTime <- getZonedTime
  case validateDateTime serverTime fTimeZone fDate fTime of
    Success _  -> pure ()
    Failure es -> throw @InvalidDateTimeError . serverError $ show es
  mMatches <- makeMatch fParticipants
  case mMatches of
    Nothing      -> throwInternalError $ noMatchesFound fParticipants
    Just matches -> forM_ matches $ sendEmail . mkMail sender f
 where
  noMatchesFound ps =
    serverError ("No matches found: " <> show ps)
      `errWhen` "running Secret Santa"

mkMail :: Sender -> Form -> (Participant, Participant) -> Mail
mkMail (Sender sender) f (gifter, receiver) =
  let toAddress =
        Address { addressName = Just gifterName, addressEmail = gifterEmail }
      fromAddress = Address { addressName  = Just "Secret Santa"
                            , addressEmail = emailAddressToText sender
                            }
      subject = "Secret Santa"
      html :: Html = [shamlet|
        <p>Hi #{gifterName},
        <p>#{hostName} invited you to <b>#{eventName}</b>:
        <p>Description:
        <p>#{description}

        <p> For this event, you are chosen to buy a present for <b>#{receiverName}</b>.
            Someone else will buy a present for you in return!

        <p>Additional info about <b>#{eventName}</b>:
          <ul>
            <li>Host: #{hostName} - #{hostEmail}
            $maybe date <- mDate 
              <li>Date: #{date}
            $maybe time <- mTime 
              <li>Time: #{time}
            $maybe location <- mLocation 
              <li>Location: #{location}
            $maybe price <- mPrice 
              <li>Price: #{price}
        |]
      plainBody = BlazeText.renderMarkup html
      htmlBody  = BlazeHtml.renderHtml html
  in  simpleMailInMemory toAddress fromAddress subject plainBody htmlBody []
 where
  Form UnsafeForm {..} = f
  eventName            = unNonEmptyText fEventName
  hostName             = unNonEmptyText fHostName
  hostEmail            = emailAddressToText fHostEmail
  mDate :: Maybe Text  = show <$> fDate
  mTime :: Maybe Text  = show <$> fTime
  mLocation            = unNonEmptyText <$> fLocation
  mPrice :: Maybe Text = show <$> fPrice
  description          = unNonEmptyText fDescription
  gifterName           = unNonEmptyText . pName $ gifter
  gifterEmail          = emailAddressToText . pEmail $ gifter
  receiverName         = unNonEmptyText . pName $ receiver
