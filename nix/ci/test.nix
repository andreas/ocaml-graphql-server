{ ocamlVersion }:

let
  lock = builtins.fromJSON (builtins.readFile ./../../flake.lock);
  src = fetchGit {
    url = with lock.nodes.nixpkgs.locked;"https://github.com/${owner}/${repo}";
    inherit (lock.nodes.nixpkgs.locked) rev;
  };
  pkgs = import "${src}/boot.nix" {
    extraOverlays = [
      (self: super: {
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}";
      })
    ];
  };

  inherit (pkgs) lib stdenv fetchTarball ocamlPackages;

in

pkgs.callPackage ./.. {
  doCheck = true;
}
