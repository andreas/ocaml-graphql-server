(** GraphQL schema constructor, document parser and query executer *)

(** Parsing of GraphQL documents. *)
module Parser : sig
  type value =
    | Null
    | Variable of string
    | Int of int
    | Float of float
    | String of string
    | Boolean of bool
    | Enum of string
    | List of value list
    | Object of key_value list
    [@@deriving sexp]

  and key_value = string * value [@@deriving sexp]

  type directive =
    {
      name : string;
      arguments : key_value list;
    }
    [@@deriving sexp]

  type fragment_spread =
    {
      name : string;
      directives : directive list;
    }
    [@@deriving sexp]

  type selection =
    | Field          of field
    | FragmentSpread of fragment_spread
    | InlineFragment of inline_fragment
    [@@deriving sexp]

  and field =
    {
      alias : string option;
      name : string;
      arguments : key_value list;
      directives : directive list;
      selection_set : selection list;
    }
    [@@deriving sexp]

  and inline_fragment =
    {
      type_condition : string option;
      directives : directive list;
      selection_set : selection list;
    }
    [@@deriving sexp]

  type fragment =
    {
      name : string;
      type_condition : string;
      directives : directive list;
      selection_set : selection list;
    }
    [@@deriving sexp]

  type typ =
    | NamedType   of string
    | ListType    of typ
    | NonNullType of typ
    [@@deriving sexp]

  type variable_definition =
    {
      name : string;
      typ : typ;
      default_value : value option;
    }
    [@@deriving sexp]

  type optype =
    | Query
    | Mutation
    | Subscription
    [@@deriving sexp]

  type operation =
    {
      optype : optype;
      name   : string option;
      variable_definitions : variable_definition list;
      directives : directive list;
      selection_set : selection list;
    }
    [@@deriving sexp]

  type definition =
    | Operation of operation
    | Fragment of fragment
    [@@deriving sexp]

  type document =
    definition list
    [@@deriving sexp]

  val parse : string -> (document, string) result
end

(** Constructing GraphQL schemas. *)
module Schema : sig

  (** {3 Base types } *)

  type 'ctx schema

  type ('ctx, 'src) field

  type ('ctx, 'src) typ

  (** {3 Constructors } *)

  val schema : fields:('ctx, unit) field list ->
               'ctx schema

  val obj : name:string ->
            fields:('ctx, 'src) field list ->
            ('ctx, 'src option) typ

  module Arg : sig
    type (_, _) arg
    type (_, _) arg_typ

    type (_, _) arg_list =
      | [] : ('a, 'a) arg_list
      | (::) : ('b, 'c -> 'b) arg * ('a, 'b) arg_list -> ('a, 'c -> 'b) arg_list

    val arg : ?default:'a option ->
              string ->
              typ:('a, 'b) arg_typ ->
              ('a, 'b) arg

    val scalar : name:string ->
                 coerce:(Parser.value -> ('b, string) result) ->
                 ('a, 'b option -> 'a) arg_typ

    val enum : name:string ->
               values:(string * 'b) list ->
               ('a, 'b option -> 'a) arg_typ

    val obj : name:string ->
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

  val field : name:string ->
              typ:('ctx, 'a) typ ->
              args:('a, 'b) Arg.arg_list ->
              resolve:('ctx -> 'src -> 'b) ->
              ('ctx, 'src) field

  val enum : name:string ->
             values:('a * string) list ->
             ('ctx, 'a option) typ

  val scalar : name:string ->
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
end

val execute : 'ctx Schema.schema -> 'ctx -> Parser.document -> Yojson.Basic.json
(** [execute schema ctx doc] evaluates the [doc] against [schema] with the
    given context [ctx]. *)
