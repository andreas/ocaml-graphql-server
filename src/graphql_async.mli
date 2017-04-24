(** GraphQL schema with Async support *)
module Schema : sig
  include Graphql_intf.Schema with type 'a io = 'a Async_kernel.Deferred.t
end
