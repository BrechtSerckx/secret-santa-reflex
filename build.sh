#! /bin/sh

nix-build -o result-backend -A ghc.backend && nix-build -o result-frontend -A ghcjs.frontend
