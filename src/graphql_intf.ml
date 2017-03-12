(** Constructing GraphQL schemas. *)
module type Schema = sig
  type +'a io

  (** {3 Base types } *)

  type 'ctx schema

  type ('ctx, 'src) field

  type ('ctx, 'src) typ

  (** {3 Constructors } *)

  val schema : ?mutation_name:string ->
               ?mutations:('ctx, unit) field list ->
               ?query_name:string ->
               ('ctx, unit) field list ->
               'ctx schema

  val obj : ?doc:string ->
            string ->
            fields:(('ctx, 'src option) typ -> ('ctx, 'src) field list) ->
            ('ctx, 'src option) typ

  module Arg : sig
    type (_, _) arg
    type (_, _) arg_typ

    type (_, _) arg_list =
      | [] : ('a, 'a) arg_list
      | (::) : ('b, 'c -> 'b) arg * ('a, 'b) arg_list -> ('a, 'c -> 'b) arg_list

    val arg : ?doc:string ->
              string ->
              typ:('a, 'b -> 'a) arg_typ ->
              ('a, 'b -> 'a) arg

    val arg' : ?doc:string ->
               string ->
               typ:('a, 'b option -> 'a) arg_typ ->
               default:'b ->
               ('a, 'b -> 'a) arg

    val scalar : ?doc:string ->
                 string ->
                 coerce:(Graphql_parser.const_value -> ('b, string) result) ->
                 ('a, 'b option -> 'a) arg_typ

    val enum : ?doc:string ->
               string ->
               values:(string * 'b) list ->
               ('a, 'b option -> 'a) arg_typ

    val obj : ?doc:string ->
              string ->
              fields:('c, 'b) arg_list ->
              coerce:'b ->
              ('a, 'c option -> 'a) arg_typ

    (* Argument constructors *)
    val int : ('a, int option -> 'a) arg_typ
    val string : ('a, string option -> 'a) arg_typ
    val bool : ('a, bool option -> 'a) arg_typ
    val float : ('a, float option -> 'a) arg_typ
    val guid : ('a, string option -> 'a) arg_typ
    val list : ('a, 'b -> 'a) arg_typ -> ('a, 'b list option -> 'a) arg_typ
    val non_null : ('a, 'b option -> 'a) arg_typ -> ('a, 'b -> 'a) arg_typ
  end

  val field : ?doc:string ->
              string ->
              typ:('ctx, 'a) typ ->
              args:('a, 'b) Arg.arg_list ->
              resolve:('ctx -> 'src -> 'b) ->
              ('ctx, 'src) field

  val io_field : ?doc:string ->
                 string ->
                 typ:('ctx, 'a) typ ->
                 args:('a io, 'b) Arg.arg_list ->
                 resolve:('ctx -> 'src -> 'b) ->
                 ('ctx, 'src) field

  val enum : ?doc:string ->
             string ->
             values:('a * string) list ->
             ('ctx, 'a option) typ

  val scalar : ?doc:string ->
               string ->
               coerce:('a -> Yojson.Basic.json) ->
               ('ctx, 'a option) typ

  val list : ('ctx, 'src) typ -> ('ctx, 'src list option) typ

  val non_null : ('ctx, 'src option) typ -> ('ctx, 'src) typ

  (** {3 Built-in scalars} *)

  val int    : ('ctx, int option) typ
  val string : ('ctx, string option) typ
  val guid   : ('ctx, string option) typ
  val bool   : ('ctx, bool option) typ
  val float  : ('ctx, float option) typ

  type variables = (string * Graphql_parser.const_value) list

  val execute : 'ctx schema -> 'ctx -> ?variables:variables -> Graphql_parser.document -> (Yojson.Basic.json, Yojson.Basic.json) result io
  (** [execute schema ctx variables doc] evaluates the [doc] against [schema]
      with the given context [ctx] and [variables]. *)
end
