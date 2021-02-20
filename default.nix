# default.nix
{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }:
  let hlib = pkgs.haskell.lib;
      sources = import nix/sources.nix;
  in {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
  useWarp = true;
  overrides = self: super: {
    servant-reflex = self.callCabal2nix "servant-reflex" sources.servant-reflex { };
    polysemy-plugin = hlib.dontCheck (hlib.markUnbroken super.polysemy-plugin);
    refined = hlib.appendConfigureFlag (hlib.markUnbroken super.refined) "-f-QuickCheck";
  };
})
