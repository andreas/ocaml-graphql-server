type const_value = [
  | `Null
  | `Int of int
  | `Float of float
  | `String of string
  | `Bool of bool
  | `Enum of string
  | `List of const_value list
  | `Assoc of (string * const_value) list
] [@@deriving show]

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
] [@@deriving show]

type directive =
  {
    name : string;
    arguments : (string * value) list;
  }
  [@@deriving show]

type fragment_spread =
  {
    name : string;
    directives : directive list;
  }
  [@@deriving show]

type selection =
  | Field of field
  | FragmentSpread of fragment_spread
  | InlineFragment of inline_fragment
  [@@deriving show]

and field =
  {
    alias : string option;
    name : string;
    arguments : (string * value) list;
    directives : directive list;
    selection_set : selection list;
  }
  [@@deriving show]

and inline_fragment =
  {
    type_condition : string option;
    directives : directive list;
    selection_set : selection list;
  }
  [@@deriving show]

type fragment =
  {
    name : string;
    type_condition : string;
    directives : directive list;
    selection_set : selection list;
  }
  [@@deriving show]

type typ =
  | NamedType of string
  | ListType of typ
  | NonNullType of typ
  [@@deriving show]

type variable_definition =
  {
    name : string;
    typ : typ;
    default_value : const_value option;
  }
  [@@deriving show]

type optype =
  | Query
  | Mutation
  | Subscription
  [@@deriving show]

type operation =
  {
    optype : optype;
    name : string option;
    variable_definitions : variable_definition list;
    directives : directive list;
    selection_set : selection list;
  }
  [@@deriving show]

type definition =
  | Operation of operation
  | Fragment of fragment
  [@@deriving show]

type document =
  definition list
  [@@deriving show]
