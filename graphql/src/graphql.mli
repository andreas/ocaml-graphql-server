(** GraphQL schema *)
module Schema : sig
  include Graphql_intf.Schema with type 'a Io.t = 'a
                               and type 'a Io.Stream.t = 'a Seq.t
end
