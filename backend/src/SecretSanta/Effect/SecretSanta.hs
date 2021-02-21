{-# LANGUAGE QuasiQuotes #-}
module SecretSanta.Effect.SecretSanta
  ( SecretSanta
  , createSecretSanta
  , runSecretSantaPrint
  , runSecretSanta
  , InternalError(..)
  ) where

import           Polysemy
import           Polysemy.Error

import           Text.NonEmpty

import qualified Data.Text.Lazy                as TL
import           Network.Mail.Mime
import qualified Text.Blaze.Html.Renderer.Text as BlazeHtml
import qualified Text.Blaze.Renderer.Text      as BlazeText
import           Text.EmailAddress
import           Text.Hamlet

import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.Match

data SecretSanta m a where
  -- | Create a new secret santa
  CreateSecretSanta ::Form -> SecretSanta m ()

data InternalError = NoMatchesFound [Participant] -- ^ No matches are found, so preconditions weren't met
  deriving Show

makeSem ''SecretSanta

runSecretSantaPrint
  :: forall r a . Sem (SecretSanta ': r) a -> Sem (Embed IO ': r) a
runSecretSantaPrint = reinterpret $ \case
  CreateSecretSanta f -> embed $ print @IO f

runSecretSanta
  :: forall r a
   . Sem (SecretSanta ': r) a
  -> Sem (Error InternalError ': Match ': Email ': r) a
runSecretSanta = reinterpret3 $ \case
  CreateSecretSanta f@(Form UnsafeForm {..}) -> do
    mMatches <- makeMatch fParticipants
    case mMatches of
      Nothing      -> throw $ NoMatchesFound fParticipants
      Just matches -> forM_ matches $ sendEmail . mkMail f

mkMail :: Form -> (Participant, Participant) -> Mail
mkMail f (gifter, receiver) =
  let to =
        Address { addressName = Just gifterName, addressEmail = gifterEmail }
      from = Address { addressName  = Just "Secret Santa"
                     , addressEmail = "secret-santa@host"
                     }
      subject = "Secret Santa"
      html :: Html = [shamlet|
        <p>Hi #{gifterName},
        <p>
          #{hostName} invited you to <b>#{eventName}</b>:
          Description: #{description}

          For this event, you are chosen to buy a present for <b>#{receiverName}</b>.
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
  in  simpleMailInMemory to from subject plainBody htmlBody []
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
