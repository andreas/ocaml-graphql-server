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
  | Field of field
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
  | NamedType of string
  | ListType of typ
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
    name : string option;
    variable_definitions : variable_definition list;
    directives : directive list;
    selection_set : selection list;
  }

type definition =
  | Operation of operation
  | Fragment of fragment

type document =
  definition list

module Pp = struct
  let comma : unit Fmt.t = Fmt.(const string ",")
  let colon : unit Fmt.t = Fmt.(const string ":")

  let quote_string s =
    let open Re in
    s
    |> replace_string (compile (char '\\'))   ~by:"\\\\"
    |> replace_string (compile (char '"'))    ~by:"\\\""
    |> replace_string (compile (char '\b'))   ~by:"\\b"
    |> replace_string (compile (char '\012')) ~by:"\\f"
    |> replace_string (compile (char '\n'))   ~by:"\\n"
    |> replace_string (compile (char '\r'))   ~by:"\\r"
    |> replace_string (compile (char '\t'))   ~by:"\\t"

  let rec pp_value fmt = function
    | `Null -> Fmt.string fmt "null"
    | `Int n -> Fmt.int fmt n
    | `Float f -> Fmt.float fmt f
    | `String s -> Fmt.(quote string) fmt (quote_string s)
    | `Bool b -> Fmt.bool fmt b
    | `Enum e -> Fmt.string fmt e
    | `Variable s -> Fmt.fmt "$%s" fmt s
    | `List l -> Fmt.(brackets (list ~sep:comma pp_value)) fmt l
    | `Assoc props -> Fmt.(braces (list ~sep:comma (pair ~sep:colon string pp_value))) fmt props

  let omit_empty_list t fmt = function
    | [] -> ()
    | xs -> t fmt xs

  let arguments fmt args =
    omit_empty_list Fmt.(parens (list ~sep:comma (pair ~sep:colon string pp_value))) fmt args

  let pp_directive fmt (directive : directive) =
    Fmt.fmt "@%s%a" fmt directive.name arguments directive.arguments

  let directives = Fmt.(list pp_directive)

  let pp_fragment_spread fmt (fragment_spread : fragment_spread) =
    Fmt.fmt "...%s%a" fmt fragment_spread.name directives fragment_spread.directives

  let rec pp_selection fmt = function
    | Field f ->
        begin match f.alias with
        | Some alias ->
            Fmt.fmt "%s: %s%a%a%a" fmt alias f.name arguments f.arguments directives f.directives selection_set f.selection_set
        | None ->
            Fmt.fmt "%s%a%a%a" fmt f.name arguments f.arguments directives f.directives selection_set f.selection_set
        end
    | FragmentSpread f ->
        Fmt.fmt "... %s %a" fmt f.name directives f.directives
    | InlineFragment f ->
        match f.type_condition with
        | Some condition ->
            Fmt.fmt "... on %s %a %a" fmt condition directives f.directives selection_set f.selection_set
        | None ->
            Fmt.fmt "... %a %a" fmt directives f.directives selection_set f.selection_set
  and selection_set fmt = omit_empty_list Fmt.(braces (hvbox ~indent:2 (prefix cut (list pp_selection)))) fmt

  let rec pp_typ fmt = function
    | NamedType t -> Fmt.string fmt t
    | ListType t -> Fmt.brackets pp_typ fmt t
    | NonNullType t -> Fmt.fmt "%a!" fmt pp_typ t

  let pp_variable_definition fmt var_def =
    match var_def.default_value with
    | None ->
        Fmt.fmt "$%s : %a" fmt var_def.name pp_typ var_def.typ
    | Some value ->
        Fmt.fmt "$%s : %a = %a" fmt var_def.name pp_typ var_def.typ pp_value value

  let variables = omit_empty_list Fmt.(parens (list ~sep:comma pp_variable_definition))

  let pp_optype fmt = function
    | Query -> Fmt.string fmt "query"
    | Mutation -> Fmt.string fmt "mutation"
    | Subscription -> Fmt.string fmt "subscription"

  let pp_operation fmt op =
    match op.name with
    | None ->
        selection_set fmt op.selection_set
    | Some name ->
        Fmt.fmt "%a %s%a%a %a" fmt pp_optype op.optype name variables op.variable_definitions directives op.directives selection_set op.selection_set

  let pp_fragment fmt (f : fragment) =
    Fmt.fmt "fragment %s on %s %a %a" fmt f.name f.type_condition directives f.directives selection_set f.selection_set

  let pp_definition fmt = function
    | Operation op -> pp_operation fmt op
    | Fragment f -> pp_fragment fmt f

  let pp_document = Fmt.(list pp_definition)
end

let pp_document = Pp.pp_document
