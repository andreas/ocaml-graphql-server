#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "graphql-server" @@ fun c ->
  Ok [ Pkg.mllib "src/graphql.mllib" ]
