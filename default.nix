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
    };

    shells = {
      ghc = [
        "common"
        "backend"
        "frontend"
        "servant-reflex"
      ];
      ghcjs = [
        "common"
        "frontend"
        "servant-reflex"
      ];
    };
    useWarp = true;
    overrides = self: super: {
      # base without prelude (needs ghcjs fix)
      base-noprelude = self.callCabal2nix "base-noprelude" sources.base-noprelude { };

      # polysemy
      polysemy = self.callCabal2nixWithOptions "polysemy" sources.polysemy "--no-hpack" {};
      polysemy-plugin = self.callCabal2nixWithOptions "polysemy-plugin" "${sources.polysemy}/polysemy-plugin" "--no-hpack" {};
      polysemy-zoo = self.callCabal2nixWithOptions "polysemy-zoo" sources.polysemy-zoo "--no-hpack" {};
      polysemy-extra = self.callCabal2nixWithOptions "polysemy-extra" sources.polysemy-extra "--no-hpack" {};

      # mail
      smtp-mail = self.callCabal2nix "smtp-mail" sources.smtp-mail { };
      mime-mail-ses = self.callCabal2nix "mime-mail-ses" sources.mime-mail-ses {};

      # co-log
      typerep-map = hlib.dontCheck(hlib.markUnbroken(super.typerep-map));
      co-log-core = self.callCabal2nixWithOptions "co-log" sources.co-log "--subpath co-log-core" {};
      co-log = hlib.doJailbreak(self.callCabal2nixWithOptions "co-log" sources.co-log "--subpath co-log" {});
      co-log-polysemy = hlib.doJailbreak(self.callCabal2nixWithOptions "co-log" sources.co-log "--subpath co-log-polysemy" {});

      # servant
      servant = self.callCabal2nix "servant" "${sources.servant}/servant" { };
      servant-server = hlib.dontCheck (self.callCabal2nix "servant-server" "${sources.servant}/servant-server" { });
      servant-foreign = self.callCabal2nix "servant-foreign" "${sources.servant}/servant-foreign" { };

      # brittany
      brittany = self.callCabal2nix "brittany" sources.brittany {};

      # beam
      beam-core = self.callCabal2nixWithOptions "beam-core" sources.beam "--subpath beam-core" {};
      beam-migrate = self.callCabal2nixWithOptions "beam-migrate" sources.beam "--subpath beam-migrate" {};
      beam-postgres = self.callCabal2nixWithOptions "beam-postgres" sources.beam "--subpath beam-postgres" {};
      beam-sqlite = self.callCabal2nixWithOptions "beam-sqlite" sources.beam "--subpath beam-sqlite" {};
    };
    shellToolOverrides = ghc: super: {
      inherit (ghc) brittany hlint;
    };
  })
