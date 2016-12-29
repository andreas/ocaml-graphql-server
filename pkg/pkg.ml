#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let lwt = Conf.with_pkg "lwt"
let async = Conf.with_pkg "async"

let () =
  Pkg.describe "graphql" @@ fun c ->
  let lwt = Conf.value c lwt in
  let async = Conf.value c async in
  Ok [
    Pkg.mllib "src/graphql.mllib";
    Pkg.mllib ~cond:lwt "src/graphql_lwt.mllib";
    Pkg.mllib ~cond:async "src/graphql_async.mllib";
    Pkg.test "test/test";
  ]
