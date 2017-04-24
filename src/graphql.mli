(** GraphQL schema *)
module Schema : sig
  include Graphql_intf.Schema with type +'a io = 'a
end
