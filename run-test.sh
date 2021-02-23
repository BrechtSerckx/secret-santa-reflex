#! /usr/bin/env nix-shell
#! nix-shell  -i bash -A shells.ghc

# run with configuration in a `.env` file, that for obvious reasons is not
# included in here
env $(cat .env | xargs) cabal run backend -- --email-backend GMail
