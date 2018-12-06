(* GraphQL schema functor *)
module Make (Io : Graphql_intf.IO) :
  Graphql_intf.Schema with module Io = Io
