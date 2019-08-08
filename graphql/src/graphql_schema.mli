(* GraphQL schema functor *)
module Make (Io : Graphql_intf.IO) (Err : Graphql_intf.Err) :
  Graphql_intf.Schema with module Io = Io and type err = Err.t
