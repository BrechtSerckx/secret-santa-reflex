module SecretSanta.Opts
  ( Opts(..)
  , parseOpts
  , EmailBackend(..)
  ) where

import qualified Options.Applicative           as OA

newtype Opts = Opts
  { oEmailBackend :: EmailBackend
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
  pure Opts { .. }

pEmailBackend :: OA.Parser EmailBackend
pEmailBackend =
  OA.option OA.auto
    . mconcat
    $ [OA.long "email-backend", OA.metavar "EMAIL_BACKEND", OA.value None]
