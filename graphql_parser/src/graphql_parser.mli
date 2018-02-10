(** GraphQL query parser *)

type const_value = [
  | `Null
  | `Int of int
  | `Float of float
  | `String of string
  | `Bool of bool
  | `Enum of string
  | `List of const_value list
  | `Assoc of (string * const_value) list
]

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
]

type directive =
  {
    name : string;
    arguments : (string * value) list;
  }

type fragment_spread =
  {
    name : string;
    directives : directive list;
  }

type selection =
  | Field          of field
  | FragmentSpread of fragment_spread
  | InlineFragment of inline_fragment

and field =
  {
    alias : string option;
    name : string;
    arguments : (string * value) list;
    directives : directive list;
    selection_set : selection list;
  }

and inline_fragment =
  {
    type_condition : string option;
    directives : directive list;
    selection_set : selection list;
  }

type fragment =
  {
    name : string;
    type_condition : string;
    directives : directive list;
    selection_set : selection list;
  }

type typ =
  | NamedType   of string
  | ListType    of typ
  | NonNullType of typ

type variable_definition =
  {
    name : string;
    typ : typ;
    default_value : const_value option;
  }

type optype =
  | Query
  | Mutation
  | Subscription

type operation =
  {
    optype : optype;
    name   : string option;
    variable_definitions : variable_definition list;
    directives : directive list;
    selection_set : selection list;
  }

type definition =
  | Operation of operation
  | Fragment of fragment

type document =
  definition list

val parse : string -> (document, string) result

val pp_document : document Fmt.t
