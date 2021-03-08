let
  sources = import ./nix/sources.nix;
  project = (import ./. {});
  pkgs = project.reflex.nixpkgs;
  backend = project.ghc.backend;
  frontend = project.ghcjs.frontend;

in pkgs.dockerTools.buildImage {
  name = "secret-santa-reflex";
  tag = "latest";
  fromImageName = "alpine:latest";
  contents = [ pkgs.cacert backend pkgs.bash pkgs.coreutils ];
  config = {
    Entrypoint = [ "${backend}/bin/backend" ];
    Cmd = [ "-h" ];
    WorkingDir = "/srv/";
    ExposedPorts = {
      "8080/tcp" = { };
    };
  };
  runAsRoot = ''
    mkdir -p /var
    cp -r ${frontend}/bin/frontend.jsexe /var/www
  '';
}
