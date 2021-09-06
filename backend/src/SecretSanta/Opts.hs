module SecretSanta.Opts
  ( ServeOpts(..)
  , parseCmd
  , Cmd(..)
  , CreateDBOpts(..)
  ) where

import           Data.Refine
import           Data.String                    ( String )
import qualified Data.Text                     as T
import           Data.Validate
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Options.Applicative           as OA
import           SecretSanta.API                ( AuthToken(..) )
import           SecretSanta.Backend.Email
import           SecretSanta.Backend.KVStore
import "common"  Text.EmailAddress

data Cmd
  = Serve ServeOpts
  | CreateDB CreateDBOpts

data ServeOpts = ServeOpts
  { soEmailBackend   :: AnyEmailBackend
  , soEmailSender    :: EmailAddress
  , soWebRoot        :: FilePath
  , soPort           :: Warp.Port
  , soKVStoreBackend :: AnyKVStoreBackend
  , soAdminToken     :: AuthToken
  }

data CreateDBOpts = CreateDBOpts
  { cdbDatabaseBackend :: AnyDatabaseBackend
  }

parseCmd :: IO Cmd
parseCmd =
  let parserInfo = mconcat [OA.fullDesc, OA.progDesc "Secret Santa server"]
  in  OA.execParser $ (pCmd <**> OA.helper) `OA.info` parserInfo

pCmd :: OA.Parser Cmd
pCmd = OA.hsubparser $ mconcat
  [ OA.command "serve"
    $ let serveInfo =
            mconcat [OA.fullDesc, OA.progDesc "Run the Secret Santa server"]
      in  Serve <$> pServeOpts `OA.info` serveInfo
  , OA.command "create-db"
    $ let createDBInfo = mconcat
            [ OA.fullDesc
            , OA.progDesc "Create an empty database for the Secret Santa server"
            ]
      in  CreateDB <$> pCreateDBOpts `OA.info` createDBInfo
  ]

pCreateDBOpts :: OA.Parser CreateDBOpts
pCreateDBOpts = do
  cdbDatabaseBackend <- parseDatabaseBackends
  pure CreateDBOpts { .. }

pServeOpts :: OA.Parser ServeOpts
pServeOpts = do
  soEmailBackend   <- pEmailBackend
  soEmailSender    <- pEmailSender
  soWebRoot        <- pWebRoot
  soPort           <- pPort
  soKVStoreBackend <- pKVStoreBackend
  soAdminToken     <- pAdminToken
  pure ServeOpts { .. }

pAdminToken :: OA.Parser AuthToken
pAdminToken = fmap AuthToken . OA.strOption $ mconcat
  [ OA.long "admin-token"
  , OA.metavar "TOKEN"
  , OA.value "i-am-ze-admin"
  , OA.showDefault
  ]

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
    AnyEmailBackend (Proxy :: Proxy eb) -> T.unpack $ emailBackendName @eb

pKVStoreBackend :: OA.Parser AnyKVStoreBackend
pKVStoreBackend = parseKVStoreBackends

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
