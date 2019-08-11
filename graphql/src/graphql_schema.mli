(* GraphQL schema functor *)
module Make (Io : Graphql_intf.IO) (Field_error : Graphql_intf.Field_error) :
  Graphql_intf.Schema with module Io = Io and type field_error = Field_error.t
