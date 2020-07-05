{ ocamlVersion }:

let
  pkgs = import ../sources.nix { inherit ocamlVersion; };
  inherit (pkgs) lib stdenv fetchTarball ocamlPackages;

  in

  import ./.. {
    inherit pkgs;
    doCheck = true;
  }

