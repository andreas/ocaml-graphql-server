{ lib, stdenv, ocamlPackages, nix-filter, doCheck ? true }:

with ocamlPackages;

let
  buildPkg = args: buildDunePackage ({
    version = "0.13.0-dev";
    doCheck = doCheck;
  } // args);

  graphqlPkgs =
    rec {
      graphql_parser = buildPkg {
        pname = "graphql_parser";
        src = with nix-filter; filter {
          root = ./..;
          include = [
            "dune-project"
            "graphql_parser"
            "graphql_parser.opam"
          ];
        };

        checkInputs = [ alcotest ];
        nativeBuildInputs = [ menhir ];
        propagatedBuildInputs = [
          menhir
          fmt
          re
        ];
      };

      graphql = buildPkg {
        pname = "graphql";
        src = with nix-filter; filter {
          root = ./..;
          include = [
            "dune-project"
            "graphql"
            "graphql.opam"
          ];
        };
        checkInputs = [ alcotest ];
        propagatedBuildInputs = [
          graphql_parser
          yojson
          rresult
          seq
        ];
      };

      graphql-lwt = buildPkg {
        pname = "graphql-lwt";
        src = with nix-filter; filter {
          root = ./..;
          include = [
            "dune-project"
            "graphql-lwt"
            "graphql-lwt.opam"
          ];
        };

        checkInputs = [ alcotest ];

        inherit doCheck;
        propagatedBuildInputs = [
          graphql
          lwt
        ];
      };

      graphql-async = buildPkg {
        pname = "graphql-async";
        src = with nix-filter; filter {
          root = ./..;
          include = [
            "dune-project"
            "graphql-async"
            "graphql-async.opam"
          ];
        };
        doCheck = false;
        propagatedBuildInputs = [
          graphql
          async
        ];
      };
    };

in

graphqlPkgs // (if lib.versionOlder "5.0" ocaml.version then {
  graphql-eio = buildPkg {
    pname = "graphql-eio";
    src = with nix-filter; filter {
      root = ./..;
      include = [
        "dune-project"
        "graphql-eio"
        "graphql-eio.opam"
      ];
    };

    propagatedBuildInputs = [
      graphql
      eio_main
    ];
    checkInputs = [ alcotest ];
  };

} else { })
