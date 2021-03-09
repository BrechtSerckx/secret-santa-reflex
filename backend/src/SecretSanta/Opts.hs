module SecretSanta.Opts
  ( Opts(..)
  , parseOpts
  , EmailBackend(..)
  ) where

import qualified Data.Text                     as T
import qualified Options.Applicative           as OA
import "common"  Text.EmailAddress
import "emailaddress" Text.EmailAddress         ( validateFromString )

data Opts = Opts
  { oEmailBackend :: EmailBackend
  , oEmailSender  :: EmailAddress
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
  pure Opts { .. }

pEmailBackend :: OA.Parser EmailBackend
pEmailBackend =
  OA.option OA.auto
    . mconcat
    $ [OA.long "email-backend", OA.metavar "EMAIL_BACKEND", OA.value None]

pEmailSender :: OA.Parser EmailAddress
pEmailSender =
  OA.option (OA.eitherReader validateFromString)
    . mconcat
    $ [OA.long "email-sender", OA.metavar "EMAIL_ADDRESS"]
