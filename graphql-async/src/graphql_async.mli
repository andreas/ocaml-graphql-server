(** GraphQL schema with Async support *)
module Schema : sig
  include Graphql_intf.Schema with type 'a Io.t = 'a Async_kernel.Deferred.t
                              and type 'a Io.Stream.t = 'a Async_kernel.Pipe.Reader.t
end
