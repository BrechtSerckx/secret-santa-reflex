module SecretSanta.Opts
  ( Opts(..)
  , parseOpts
  ) where

import           Data.Refine
import           Data.String                    ( String )
import qualified Data.Text                     as T
import           Data.Validate
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Options.Applicative           as OA
import           SecretSanta.Backend.Email
import           SecretSanta.Backend.Email.Dummy
                                                ( )
import           SecretSanta.Backend.Email.GMail
                                                ( )
import           SecretSanta.Backend.Email.SES  ( )
import           SecretSanta.Backend.KVStore
import           SecretSanta.Backend.KVStore.Database
                                                ( )
import           SecretSanta.Backend.KVStore.State
                                                ( KVConfig(..) )
import           SecretSanta.Store
import "common"  Text.EmailAddress

data Opts = Opts
  { oEmailBackend :: AnyEmailBackend
  , oEmailSender  :: EmailAddress
  , oWebRoot      :: FilePath
  , oPort         :: Warp.Port
  , oKVBackend    :: AnyKVBackendWithConfig Stores
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
  oKVBackend    <- pKVBackend
  pure Opts { .. }


pEmailBackend :: OA.Parser AnyEmailBackend
pEmailBackend =
  OA.option readEmailBackend
    . mconcat
    $ [ OA.long "email-backend"
      , OA.metavar "EMAIL_BACKEND"
      , OA.value (AnyEmailBackend $ Proxy @Dummy)
      , OA.showDefaultWith showEmailBackend
      ]
 where
  readEmailBackend = OA.maybeReader $ readEmailBackends . T.pack
  showEmailBackend :: AnyEmailBackend -> String
  showEmailBackend = \case
    AnyEmailBackend (Proxy :: Proxy eb)  -> T.unpack $ emailBackendName @eb

pKVBackend :: OA.Parser (AnyKVBackendWithConfig Stores)
pKVBackend =
  let pState = OA.flag' (AnyKVBackendWithConfig SKVState KVStateConfig)
        $ mconcat [OA.long "in-memory"]
      pDatabase =
        fmap (AnyKVBackendWithConfig SKVDatabase) . OA.strOption $ mconcat
          [OA.long "sqlite", OA.metavar "SQLITE_DATABASE"]
  in  pState <|> pDatabase

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
