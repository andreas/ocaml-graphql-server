{ pkgs }:

let
  inherit (pkgs) lib;
  graphqlPkgs = pkgs.recurseIntoAttrs (pkgs.callPackage ./nix { });
  graphqlDrvs = lib.filterAttrs (_: value: lib.isDerivation value) graphqlPkgs;

  filterDrvs = inputs:
    lib.filter
      (drv:
        # we wanna filter our own packages so we don't build them when entering
        # the shell. They always have `pname`
        !(lib.hasAttr "pname" drv) ||
        drv.pname == null ||
        !(lib.any (name: name == drv.pname || name == drv.name) (lib.attrNames graphqlDrvs)))
      inputs;

in
with pkgs;

(mkShell {
  inputsFrom = lib.attrValues graphqlDrvs;
  buildInputs = with ocamlPackages; [ ocamlformat utop crunch digestif base64 cohttp ];
}).overrideAttrs (o: {
  propagatedBuildInputs = filterDrvs o.propagatedBuildInputs;
  buildInputs = filterDrvs o.buildInputs;
})
