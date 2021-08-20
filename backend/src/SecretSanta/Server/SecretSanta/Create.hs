{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module SecretSanta.Server.SecretSanta.Create
  ( createSecretSantaHandler
  , InvalidDateTimeError
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Extra
import           Polysemy.Fresh
import           Polysemy.Input
import           Polysemy.Operators
import           Polysemy.Transaction.Beam

import           Data.Error
import           Data.Refine
import           Data.SOP
import           Data.Time
import           Data.Time.MonadTime
import qualified Data.UUID.V4                  as UUID
import           Data.Validate
import           Text.NonEmpty

import           Network.Mail.Mime
import qualified Text.Blaze.Html.Renderer.Text as BlazeHtml
import qualified Text.Blaze.Renderer.Text      as BlazeText
import           Text.Hamlet

import           SecretSanta.API
import           SecretSanta.Backend.KVStore
import           SecretSanta.Data
import           SecretSanta.Effect.Email
import           SecretSanta.Effect.Match
import           SecretSanta.Effect.Time

import           Servant.API.UVerb
import qualified Servant.Server                as SS

createSecretSantaHandler
  :: forall kv r
   . ( Members
         '[ Input Sender
          , Email
          , GetTime
          , KVStoreInit kv SecretSantaStore
          , Embed IO
          , Input (KVConnection kv)
          ]
         r
     , RunKVStore kv SecretSantaStore
     )
  => SecretSanta
  -> Error InternalError ': r @> Union '[WithStatus 200 SecretSantaId, InvalidDateTimeError]
createSecretSantaHandler ss = do
  env :: Envelope '[InvalidDateTimeError] SecretSantaId <-
    runKVTransaction @kv
    . runErrorsU @'[InvalidDateTimeError]
    . rotateEffects2
    . runKVStore @kv @SecretSantaStore
    . raiseUnder @(KVTransaction kv)
    . runFreshSecretSantaId
    . runMatchRandom
    $ createSecretSanta ss
  case env of
    Left  e -> pure $ S e
    Right r -> SS.respond $ WithStatus @200 r

runFreshSecretSantaId :: Fresh SecretSantaId ': r @> a -> IO ~@ r @> a
runFreshSecretSantaId = interpret $ \case
  Fresh -> SecretSantaId <$> embed UUID.nextRandom

createSecretSanta
  :: forall r
   . Members
       '[ Input Sender
        , Error InvalidDateTimeError
        , Match
        , Fresh SecretSantaId
        , Email
        , GetTime
        , SecretSantaStore
        ]
       r
  => SecretSanta
  -> r @> SecretSantaId
createSecretSanta ss@(SecretSanta UnsafeSecretSanta {..}) = do
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
      id <- fresh
      writeSecretSanta id ss
      forM_ matches $ sendEmail . mkMail sender secretsantaInfo
      pure id
 where
  noMatchesFound ps =
    serverError ("No matches found: " <> show ps)
      `errWhen` "running Secret Santa"

mkMail :: Sender -> Info -> (Participant, Participant) -> Mail
mkMail (Sender sender) Info {..} (gifter, receiver) =
  let toAddress =
        Address { addressName = Just gifterName, addressEmail = gifterEmail }
      fromAddress = Address { addressName  = Just "Secret Santa"
                            , addressEmail = unrefine sender
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
  hostEmail            = unrefine iHostEmail
  mDate :: Maybe Text  = show <$> iDate
  mTime :: Maybe Text  = show <$> iTime
  mLocation            = unNonEmptyText <$> iLocation
  mPrice :: Maybe Text = show <$> iPrice
  description          = unNonEmptyText iDescription
  gifterName           = unNonEmptyText . pName $ gifter
  gifterEmail          = unrefine . pEmail $ gifter
  receiverName         = unNonEmptyText . pName $ receiver
