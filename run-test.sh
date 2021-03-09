#! /usr/bin/env nix-shell
#! nix-shell  -i bash -A shells.ghc

FLAGS=()

# FLAGS+=(--email-backend None)
# FLAGS+=(--email-backend GMail)
FLAGS+=(--email-backend SES)

# run with configuration in a `.env` file, that for obvious reasons is not
# included in here
env $(cat .env | xargs) cabal run backend -- "${FLAGS[@]}"
