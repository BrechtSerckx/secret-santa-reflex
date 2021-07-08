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
      servant-reflex = ./servant-reflex;
      beam-core = ./beam/beam-core;
      beam-migrate = ./beam/beam-migrate;
      beam-sqlite = ./beam/beam-sqlite;
    };

    shells = {
      ghc = [
        "common"
        "backend"
        "frontend"
        "servant-reflex"
        "beam-core"
        "beam-migrate"
        "beam-sqlite"
      ];
      ghcjs = [
        "common"
        "frontend"
        "servant-reflex"
      ];
    };
    useWarp = true;
    overrides = self: super: {
      base-noprelude = self.callCabal2nix "base-noprelude" sources.base-noprelude { };
      polysemy = self.callCabal2nixWithOptions "polysemy" sources.polysemy "--no-hpack" {};
      polysemy-plugin = self.callCabal2nixWithOptions "polysemy-plugin" "${sources.polysemy}/polysemy-plugin" "--no-hpack" {};
      polysemy-zoo = self.callCabal2nixWithOptions "polysemy-zoo" sources.polysemy-zoo "--no-hpack" {};
      polysemy-extra = self.callCabal2nixWithOptions "polysemy-extra" sources.polysemy-extra "--no-hpack" {};

      smtp-mail = self.callCabal2nix "smtp-mail" sources.smtp-mail { };

      # override servant version
      servant = self.callCabal2nix "servant" "${sources.servant}/servant" { };
      servant-server = hlib.dontCheck (self.callCabal2nix "servant-server" "${sources.servant}/servant-server" { });
      servant-foreign = self.callCabal2nix "servant-foreign" "${sources.servant}/servant-foreign" { };

      # # stuff for nri-env-parser, sucks that it requires these deps
      # # I'll have to build my own alternative, with blackjack and hookers!
      # nri-env-parser = hlib.doJailbreak (self.callCabal2nix "nri-env-parser"
      #   "${sources.haskell-libraries}/nri-env-parser" { });
      # nri-prelude = hlib.doJailbreak (hlib.dontCheck
      #   (self.callCabal2nix "nri-prelude"
      #     "${sources.haskell-libraries}/nri-prelude" { }));
      # junit-xml = self.callCabal2nix "junit-xml" sources.junit-xml { };
      # pretty-diff = self.callCabal2nix "pretty-diff" sources.pretty-diff { };
      # tasty-test-reporter =
      #   self.callCabal2nix "tasty-test-reporter" sources.tasty-test-reporter
      #   { };
      brittany = self.callCabal2nix "brittany" sources.brittany {};
      # beam-migrate = hlib.doJailbreak super.beam-migrate;
      # beam-core = self.callCabal2nix "beam-core" /home/brecht/code/beam/beam-core {};
      # beam-migrate = hlib.doJailbreak (self.callCabal2nix "beam-migrate" /home/brecht/code/beam/beam-migrate {});
      # beam-sqlite = self.callCabal2nix "beam-sqlite" /home/brecht/code/beam/beam-sqlite {};
    };
    shellToolOverrides = ghc: super: {
      inherit (ghc) brittany hlint;
    };
  })
