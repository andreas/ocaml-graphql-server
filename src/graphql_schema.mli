(* IO *)
module type IO = sig
  type +'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* Schema *)
module Make(Io : IO) : Graphql_intf.Schema with type 'a io = 'a Io.t