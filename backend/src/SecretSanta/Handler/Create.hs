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
import           Data.Refine
import           Data.Time
import           Data.Time.MonadTime
import           Data.Validate
import           Text.NonEmpty

import           Network.Mail.Mime
import qualified Text.Blaze.Html.Renderer.Text as BlazeHtml
import qualified Text.Blaze.Renderer.Text      as BlazeText
import           Text.Hamlet

import           SecretSanta.API
import           SecretSanta.DB
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
  => SecretSanta
  -> Sem r ()
createSecretSantaHandler ss@(SecretSanta UnsafeSecretSanta {..}) = do
  let Info {..}    = secretsantaInfo
      participants = secretsantaParticipants
  sender     <- input @Sender
  serverTime <- getZonedTime
  case validateDateTime serverTime iTimeZone iDate iTime of
    Success _  -> pure ()
    Failure es -> throw @InvalidDateTimeError . serverError $ show es
  mMatches <- makeMatch participants
  case mMatches of
    Nothing      -> throwInternalError $ noMatchesFound participants
    Just matches -> do
      embed . withConn $ \conn -> runInsertSecretSanta conn 0 ss
      forM_ matches $ sendEmail . mkMail sender secretsantaInfo
 where
  noMatchesFound ps =
    serverError ("No matches found: " <> show ps)
      `errWhen` "running Secret Santa"

mkMail :: Sender -> Info -> (Participant, Participant) -> Mail
mkMail (Sender sender) Info {..} (gifter, receiver) =
  let toAddress =
        Address { addressName = Just gifterName, addressEmail = gifterEmail }
      fromAddress = Address { addressName  = Just "Secret Santa"
                            , addressEmail = rdeconstruct sender
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
  eventName            = unNonEmptyText iEventName
  hostName             = unNonEmptyText iHostName
  hostEmail            = rdeconstruct iHostEmail
  mDate :: Maybe Text  = show <$> iDate
  mTime :: Maybe Text  = show <$> iTime
  mLocation            = unNonEmptyText <$> iLocation
  mPrice :: Maybe Text = show <$> iPrice
  description          = unNonEmptyText iDescription
  gifterName           = unNonEmptyText . pName $ gifter
  gifterEmail          = rdeconstruct . pEmail $ gifter
  receiverName         = unNonEmptyText . pName $ receiver
