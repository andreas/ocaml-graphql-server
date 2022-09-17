(** GraphQL schema with Lwt support *)
module Schema : sig
  include
    Graphql_intf.Schema
      with type 'a Io.t = 'a Eio.Promise.t
       and type 'a Io.Stream.t = 'a Seq.t
       and type field_error = string
end
