# default.nix
{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }:
  let
    hlib = pkgs.haskell.lib;
    sources = import nix/sources.nix;
  in {
    packages = {
      common = ./common;
      backend = ./backend;
      frontend = ./frontend;
      base-noprelude = ./base-noprelude;
      emailaddress = ./emailaddress;
      servant-reflex = ./servant-reflex;
    };

    shells = {
      ghc = [
        "common"
        "backend"
        "frontend"
        "base-noprelude"
        "emailaddress"
        "servant-reflex"
      ];
      ghcjs = [
        "common"
        "frontend"
        "base-noprelude"
        "emailaddress"
        "servant-reflex"
      ];
    };
    useWarp = true;
    overrides = self: super: {
      polysemy-plugin =
        hlib.dontCheck (hlib.markUnbroken super.polysemy-plugin);
      smtp-mail = self.callCabal2nix "smtp-mail" sources.smtp-mail { };

      # override servant version
      servant = self.callCabal2nix "servant" "${sources.servant}/servant" { };
      servant-server = hlib.dontCheck (self.callCabal2nix "servant-server" "${sources.servant}/servant-server" { });
      servant-foreign = self.callCabal2nix "servant-foreign" "${sources.servant}/servant-foreign" { };

      # stuff for nri-env-parser, sucks that it requires these deps
      # I'll have to build my own alternative, with blackjack and hookers!
      nri-env-parser = hlib.doJailbreak (self.callCabal2nix "nri-env-parser"
        "${sources.haskell-libraries}/nri-env-parser" { });
      nri-prelude = hlib.doJailbreak (hlib.dontCheck
        (self.callCabal2nix "nri-prelude"
          "${sources.haskell-libraries}/nri-prelude" { }));
      junit-xml = self.callCabal2nix "junit-xml" sources.junit-xml { };
      pretty-diff = self.callCabal2nix "pretty-diff" sources.pretty-diff { };
      tasty-test-reporter =
        self.callCabal2nix "tasty-test-reporter" sources.tasty-test-reporter
        { };
      brittany = self.callCabal2nix "brittany" sources.brittany {};
      beam-migrate = hlib.doJailbreak super.beam-migrate;
    };
    shellToolOverrides = ghc: super: {
      inherit (ghc) brittany hlint;
    };
  })
