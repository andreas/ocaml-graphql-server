(** GraphQL schema with Lwt support *)
module Schema : sig
  include Graphql_intf.Schema with type 'a io = 'a Lwt.t
                              and type 'a stream = 'a Lwt_stream.t * (unit -> unit)
end

module Server : sig
  val start : ?port:int -> ctx:(Cohttp.Request.t -> 'ctx) -> 'ctx Schema.schema -> unit Lwt.t
end
