(** GraphQL schema *)
module Schema : sig
  include Graphql_intf.Schema with type +'a io = 'a
                              and type 'a stream = 'a Seq.t
end
