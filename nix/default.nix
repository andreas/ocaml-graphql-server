{ pkgs ? import ./sources.nix {}, doCheck ? false, ocamlVersion ? "4_10" }:

let
  inherit (pkgs) lib stdenv ocamlPackages;
in

  with ocamlPackages;

  let buildPkg = args: buildDunePackage ({
    src = lib.gitignoreSource ./..;
    version = "0.13.0-dev";
    doCheck = false;
  } // args);

  in

  rec {
    graphql_parser = buildPkg {
      pname = "graphql_parser";
      propagatedBuildInputs = [
        menhir
        fmt
        re
      ];
    };

    graphql = buildPkg {
      pname = "graphql";
      propagatedBuildInputs = [
        graphql_parser
        yojson
        rresult
        seq
      ];
    };

    graphql-lwt = buildPkg {
      pname = "graphql-lwt";

      buildInputs = [ alcotest ];

      inherit doCheck;
      propagatedBuildInputs = [
        graphql
        lwt
      ];
    };

  }
