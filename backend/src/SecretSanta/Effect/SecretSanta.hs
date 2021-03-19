{-# LANGUAGE QuasiQuotes #-}
module SecretSanta.Effect.SecretSanta
  ( SecretSanta
  , createSecretSanta
  , runSecretSantaPrint
  , runSecretSanta
  , InvalidDateTimeError
  ) where

import           Polysemy
import           Polysemy.Error

import           Data.Error
import           Data.Time.MonadTime
import           Data.Validate
import           Text.NonEmpty

import           Network.Mail.Mime
import qualified Text.Blaze.Html.Renderer.Text as BlazeHtml
import qualified Text.Blaze.Renderer.Text      as BlazeText
import "common"  Text.EmailAddress
import           Text.Hamlet

import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.Match
import           SecretSanta.Effect.Time        ( GetTime )

data SecretSanta m a where
  -- | Create a new secret santa
  CreateSecretSanta ::Form -> SecretSanta m ()


makeSem ''SecretSanta

runSecretSantaPrint
  :: forall r a . Sem (SecretSanta ': r) a -> Sem (Embed IO ': r) a
runSecretSantaPrint = reinterpret $ \case
  CreateSecretSanta f -> embed $ print @IO f

type InvalidDateTimeError = ServerError 400 "INVALID_DATE_TIME"

runSecretSanta
  :: forall r a
   . Members '[Embed IO , GetTime , Error InvalidDateTimeError] r
  => EmailAddress
  -> Sem (SecretSanta ': r) a
  -> Sem (Match ': Email ': r) a
runSecretSanta sender = reinterpret2 $ \case
  CreateSecretSanta f@(Form UnsafeForm {..}) -> do
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

mkMail :: EmailAddress -> Form -> (Participant, Participant) -> Mail
mkMail sender f (gifter, receiver) =
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
