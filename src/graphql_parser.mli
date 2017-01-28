type const_value = [
  | `Null
  | `Int of int
  | `Float of float
  | `String of string
  | `Bool of bool
  | `Enum of string
  | `List of const_value list
  | `Assoc of (string * const_value) list
] [@@deriving sexp]

type value = [
  | `Null
  | `Int of int
  | `Float of float
  | `String of string
  | `Bool of bool
  | `Enum of string
  | `Variable of string
  | `List of value list
  | `Assoc of (string * value) list
] [@@deriving sexp]

type directive =
  {
    name : string;
    arguments : (string * value) list;
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
    arguments : (string * value) list;
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
    default_value : const_value option;
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
