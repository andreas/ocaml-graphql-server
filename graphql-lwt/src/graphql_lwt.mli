(** GraphQL schema with Lwt support *)
module Schema : sig
  include Graphql_intf.Schema with type 'a Io.t = 'a Lwt.t
                               and type 'a Io.Stream.t = 'a Lwt_stream.t * (unit -> unit)
end
