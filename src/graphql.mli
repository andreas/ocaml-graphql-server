(** GraphQL schema constructor, document parser and query executer *)

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

  val field : name:string ->
              typ:('ctx, 'a) typ ->
              resolve:('ctx -> 'src -> 'a) ->
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
  val bool   : ('ctx, bool option) typ
  val float  : ('ctx, float option) typ
end

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

val execute : 'ctx Schema.schema -> 'ctx -> Parser.document -> Yojson.Basic.json
(** [execute schema ctx doc] evaluates the [doc] against [schema] with the
    given context [ctx]. *)
