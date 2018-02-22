(** GraphQL schema with Lwt support *)
module Schema : sig
  include Graphql_intf.Schema with type 'a io = 'a Lwt.t
end

module Server : sig
  val start : ?port:int -> ctx:(unit -> 'ctx) -> 'ctx Schema.schema -> unit Lwt.t
end
