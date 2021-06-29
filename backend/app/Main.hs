module Main
  ( main
  ) where

import           SecretSanta.Server             ( secretSantaServer )

main :: IO ()
main = secretSantaServer
