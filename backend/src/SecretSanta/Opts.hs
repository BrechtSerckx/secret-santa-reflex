module SecretSanta.Opts
  ( Opts(..)
  , parseOpts
  , EmailBackend(..)
  ) where

import qualified Data.Text as T
import Data.Refine
import Data.Validate
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Options.Applicative           as OA
import "common"  Text.EmailAddress

data Opts = Opts
  { oEmailBackend :: EmailBackend
  , oEmailSender  :: EmailAddress
  , oWebRoot      :: FilePath
  , oPort         :: Warp.Port
  }

data EmailBackend = None | GMail | SES
  deriving (Eq, Show, Read)

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

pEmailBackend :: OA.Parser EmailBackend
pEmailBackend =
  OA.option OA.auto
    . mconcat
    $ [ OA.long "email-backend"
      , OA.metavar "EMAIL_BACKEND"
      , OA.value None
      , OA.showDefault
      ]

pEmailSender :: OA.Parser EmailAddress
pEmailSender =
  OA.option (OA.eitherReader parseEmailAddress)
    . mconcat
    $ [OA.long "email-sender", OA.metavar "EMAIL_ADDRESS"]
  where parseEmailAddress s = case refine $ T.pack s of
          Success a -> Right a
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
