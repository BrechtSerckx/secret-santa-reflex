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
import           Polysemy.Log
import           Polysemy.Operators

import           Data.Refine
import           Data.SOP
import           Data.Time
import           Data.Time.MonadTime
import qualified Data.UUID.V4                  as UUID
import           Data.Validate
import           Network.Http.Error
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
          , Input (KVStoreConnection kv)
          , Log Message
          ]
         r
     , RunKVStore kv SecretSantaStore
     )
  => SecretSanta
  -> Error InternalError ': r @> Union '[WithStatus 200 SecretSantaId, InvalidDateTimeError]
createSecretSantaHandler ss = do
  env :: Envelope '[InvalidDateTimeError] SecretSantaId <-
    runKVStoreTransaction @kv
    . runErrorsU @'[InvalidDateTimeError]
    . rotateEffects2
    . runKVStore @kv @SecretSantaStore
    . raiseUnder @(KVStoreTransaction kv)
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
        , Log Message
        ]
       r
  => SecretSanta
  -> r @> SecretSantaId
createSecretSanta ss@(SecretSanta UnsafeSecretSanta {..}) = do
  logDebug "Creating Secret Santa"
  let Info {..} = info
  logDebug "Validating date"
  serverTime <- getZonedTime
  case validateDateTime serverTime timeZone mDate mTime of
    Success _  -> logDebug "Validation successful"
    Failure es -> do
      logWarning "Validation failed"
      throw @InvalidDateTimeError . ApiError . mkGenericError $ show es
  logDebug "Matching participants"
  mMatches <- makeMatch participants
  id       <- case mMatches of
    Nothing -> do
      logError "Match not found"
      throwErrorPure $ noMatchesFound participants
    Just matches -> do
      logDebug "Match found"
      logDebug "Storing in database"
      id <- fresh
      createCrud id ss
      logDebug "Sending mails to participants"
      sender <- input @Sender
      forM_ matches $ sendEmail . mkMail sender info
      pure id
  logDebug "Created Secret Santa"
  pure id
 where
  noMatchesFound ps =
    mkError ("No matches found: " <> show ps) `errWhen` "running Secret Santa"

mkMail :: Sender -> Info -> (Participant, Participant) -> Mail
mkMail (Sender sender) Info {..} (gifter, receiver) =
  let toAddress = Address { addressName  = Just . unNonEmptyText $ name gifter
                          , addressEmail = unrefine $ email gifter
                          }
      fromAddress = Address { addressName  = Just "Secret Santa"
                            , addressEmail = unrefine sender
                            }
      subject = "Secret Santa"
      html :: Html = [shamlet|
        <p>Hi #{unNonEmptyText $ name gifter},
        <p>#{unNonEmptyText hostName} invited you to <b>#{unNonEmptyText eventName}</b>:
        <p>Description:
        <p>#{unNonEmptyText description}

        <p> For this event, you are chosen to buy a present for <b>#{unNonEmptyText $ name receiver}</b>.
            Someone else will buy a present for you in return!

        <p>Additional info about <b>#{unNonEmptyText eventName}</b>:
          <ul>
            <li>Host: #{unNonEmptyText hostName} - #{unrefine hostEmail}
            $maybe date <- mDate 
              <li>Date: #{showt date}
            $maybe time <- mTime 
              <li>Time: #{showt time}
            $maybe location <- mLocation 
              <li>Location: #{unNonEmptyText location}
            $maybe price <- mPrice 
              <li>Price: #{showt price}
        |]
      plainBody = BlazeText.renderMarkup html
      htmlBody  = BlazeHtml.renderHtml html
  in  simpleMailInMemory toAddress fromAddress subject plainBody htmlBody []
 where
  showt :: Show a => a -> Text
  showt = show
