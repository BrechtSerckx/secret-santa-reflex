{
  nativeCompiler ? "ghc",
  # ^ Choose a GHC for compiling backend components (web server, selenium)
  # (ghc, ghc8_0, ghc8_2, ghc8_4 or ghcHEAD)

  jsCompiler     ? "ghcjs"
  # ^ Choose a GHCJS for compiling the frontend
  # (ghcjs, ghcjs8_0, ghcjs8_2 or ghcjs8_4)

}:

let
  sources.servant = builtins.fetchGit {
    url = "https://github.com/haskell-servant/servant.git";
    rev = "0ad2bd221ad390a442f740549a502d9524b2d5d5";
  };

  reflexPlatform = import ./nix/reflex-platform.nix;

  lib = reflexPlatform.nixpkgs.haskell.lib;
  do  = funs: pkg: builtins.foldl' (a: b: b a) pkg funs;

  ghcjsPkgs = with lib; reflexPlatform.${jsCompiler}.override {
    overrides = self: super: {
      http-media      = dontCheck super.http-media;
      # servant         = dontCheck super.servant;
      lens-aeson      = dontCheck super.lens-aeson;
      servant-reflex = lib.appendConfigureFlag
                         (self.callPackage ./default.nix {}) "-fExample";
      servant = self.callCabal2nix "servant" "${sources.servant}/servant" { };
      servant-server = hlib.dontCheck (self.callCabal2nix "servant-server" "${sources.servant}/servant-server" { });
      servant-foreign = self.callCabal2nix "servant-foreign" "${sources.servant}/servant-foreign" { };
    };
  };

  ghcPkgs = with lib; reflexPlatform.${nativeCompiler}.override {
    overrides = self: super: {
      servant-snap    = doJailbreak (dontCheck ((import ./nix/servant-snap.nix {}) self super));
      testdriver      = self.callCabal2nix "testdriver" ./testdriver {};
      testserver      = import nix/testserver.nix ghcjsPkgs.servant-reflex self super;
      servant-reflex  = self.callPackage ./default.nix {};
      servant = self.callCabal2nix "servant" "${sources.servant}/servant" { };
      servant-server = hlib.dontCheck (self.callCabal2nix "servant-server" "${sources.servant}/servant-server" { });
      servant-foreign = self.callCabal2nix "servant-foreign" "${sources.servant}/servant-foreign" { };
    };
  };

  testresults = import ./nix/testresults.nix
    { inherit reflexPlatform;
      inherit (ghcPkgs) testserver testdriver;
      inherit (reflexPlatform.nixpkgs) curl;
      phantomjs = reflexPlatform.nixpkgs.phantomjs2;
    };

in
  rec {

    inherit reflexPlatform ghcPkgs ghcjsPkgs;

    build       = ghcjsPkgs.servant-reflex;
    buildGhc    = ghcPkgs.servant-reflex;
    testserver  = ghcPkgs.testserver;
    testdriver  = ghcPkgs.testdriver;
    inherit testresults;

    cabalBuild = reflexPlatform.${jsCompiler}.shellFor {
      name = "servant-reflex-cabal-builder";

      packages = p: [
        build
      ];

      shellHook = ''
        cabal configure --ghcjs -f Example
        cabal build
        exec/toSite.sh
        exit $?
      '';
    };
  }
