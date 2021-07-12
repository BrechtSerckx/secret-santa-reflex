module SecretSanta.Opts
  ( Opts(..)
  , parseOpts
  , EmailBackend(..)
  , SEmailBackend(..)
  , AnyEmailBackend(..)
  , RunEmailBackend(..)
  ) where

import           Data.Refine
import           Data.String                    ( String )
import qualified Data.Text                     as T
import           Data.Validate
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Options.Applicative           as OA
import           Polysemy
import           Polysemy.Input
import           Polysemy.Input.Env
import           Polysemy.Operators
import           SecretSanta.Effect.Email
import "common"  Text.EmailAddress

data Opts = Opts
  { oEmailBackend :: AnyEmailBackend
  , oEmailSender  :: EmailAddress
  , oWebRoot      :: FilePath
  , oPort         :: Warp.Port
  }

parseOpts :: IO Opts
parseOpts =
  let parserInfo = mconcat [OA.fullDesc, OA.progDesc "Secret Santa server"]
  in  OA.execParser $ (pOpts <**> OA.helper) `OA.info` parserInfo

pOpts :: OA.Parser Opts
pOpts = do
  oEmailBackend <- pEmailBackend
  oEmailSender  <- pEmailSender
  oWebRoot      <- pWebRoot
  oPort         <- pPort
  pure Opts { .. }

data EmailBackend = None | GMail | SES
  deriving (Eq, Show, Read)

data SEmailBackend eb where
  SNone ::SEmailBackend 'None
  SGMail ::SEmailBackend 'GMail
  SSES ::SEmailBackend 'SES
data AnyEmailBackend where
  AnyEmailBackend ::RunEmailBackend eb => SEmailBackend eb ->AnyEmailBackend

class RunEmailBackend (eb:: EmailBackend) where
  type EmailBackendConfig eb :: *
  type EmailBackendConfig eb = ()
  runEmailBackend
    :: Member (Embed IO) r
    => SEmailBackend eb
    -> Email ': r @> a
    -> Input (EmailBackendConfig eb) ': r @> a
  runEmailBackendConfig
    :: Member (Embed IO) r
    => SEmailBackend eb
    -> Input (EmailBackendConfig eb) ': r @> a
    -> r @> a
instance RunEmailBackend 'None where
  runEmailBackend SNone = raise . runEmailPrint
  runEmailBackendConfig SNone = runInputConst ()
instance RunEmailBackend 'GMail where
  type EmailBackendConfig 'GMail = GmailSettings
  runEmailBackend SGMail = runEmailGmail
  runEmailBackendConfig SGMail = runInputEnv
instance RunEmailBackend 'SES where
  type EmailBackendConfig 'SES = SESSettings
  runEmailBackend SSES = runEmailSES
  runEmailBackendConfig SSES = runInputEnv

pEmailBackend :: OA.Parser AnyEmailBackend
pEmailBackend =
  OA.option readEmailBackend
    . mconcat
    $ [ OA.long "email-backend"
      , OA.metavar "EMAIL_BACKEND"
      , OA.value (AnyEmailBackend SNone)
      , OA.showDefaultWith showEmailBackend
      ]
 where
  readEmailBackend = OA.maybeReader $ \case
    "none"  -> Just $ AnyEmailBackend SNone
    "gmail" -> Just $ AnyEmailBackend SGMail
    "ses"   -> Just $ AnyEmailBackend SSES
    _       -> Nothing
  showEmailBackend :: AnyEmailBackend -> String
  showEmailBackend = \case
    AnyEmailBackend SNone  -> "none"
    AnyEmailBackend SGMail -> "gmail"
    AnyEmailBackend SSES   -> "ses"

pEmailSender :: OA.Parser EmailAddress
pEmailSender =
  OA.option (OA.eitherReader parseEmailAddress)
    . mconcat
    $ [OA.long "email-sender", OA.metavar "EMAIL_ADDRESS"]
 where
  parseEmailAddress s = case refine $ T.pack s of
    Success a  -> Right a
    Failure es -> Left $ displayException es

pWebRoot :: OA.Parser FilePath
pWebRoot =
  OA.strOption
    . mconcat
    $ [ OA.long "web-root"
      , OA.metavar "WEB_ROOT"
      , OA.value "/var/www"
      , OA.showDefault
      ]

pPort :: OA.Parser Warp.Port
pPort =
  OA.option OA.auto
    . mconcat
    $ [ OA.long "port"
      , OA.short 'p'
      , OA.metavar "PORT"
      , OA.value 8080
      , OA.showDefault
      ]
