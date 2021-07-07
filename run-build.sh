#! /bin/sh

# build frontend
FRONTEND=$(nix-build -o result-frontend -A ghcjs.frontend)

# build backend
BACKEND=$(nix-build -o result-backend -A ghc.backend)

firefox --new-tab "$FRONTEND"/bin/frontend.jsexe/index.html
"$BACKEND"/bin/backend
