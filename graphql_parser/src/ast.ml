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
  let fprintf = Format.fprintf
  let list = Format.pp_print_list
  let comma ppf () = fprintf ppf ","
  let comma_separated ppf = list ppf ~pp_sep:comma

  let quote_string str =
    let open Str in
    str
    |> global_replace (regexp "\\") "\\\\\\\\"
    |> global_replace (regexp "\"") "\\\""
    |> global_replace (regexp "\b") "\\b"
    |> global_replace (regexp "\012") "\\f"
    |> global_replace (regexp "\n") "\\n"
    |> global_replace (regexp "\r") "\\r"
    |> global_replace (regexp "\t") "\\t"

  let rec pp_value ppf = function
    | `Null -> fprintf ppf "null"
    | `Int n -> fprintf ppf "%d" n
    | `Float f -> fprintf ppf "%f" f
    | `String s -> fprintf ppf {|"%s"|} (quote_string s)
    | `Bool b -> fprintf ppf "%b" b
    | `Enum e -> fprintf ppf "%s" e
    | `Variable s -> fprintf ppf "$%s" s
    | `List l -> fprintf ppf "[%a]" (comma_separated pp_value) l
    | `Assoc props -> fprintf ppf "{%a}" (comma_separated key_value) props

  and key_value ppf (key, value) =
    fprintf ppf "%s: %a" key pp_value value

  let omit_empty_list pp_list = function
    | [] -> ()
    | xs -> pp_list xs

  let pp_arguments ppf =
    omit_empty_list (fprintf ppf "(%a)" (comma_separated key_value))

  let pp_directive ppf ({name; arguments} : directive) =
    fprintf ppf "%@%s%a" name pp_arguments arguments

  let pp_directives = list pp_directive

  let pp_fragment_spread ppf ({name; directives} : fragment_spread) =
    fprintf ppf "...%s%a" name pp_directives directives

  let pp_option pp_value _ = function
    | None -> ()
    | Some x -> pp_value x

  let rec pp_selection ppf = function
    | Field {name; arguments; directives; selection_set; alias} ->
        fprintf ppf "%a%s%a%a%a"
          (pp_option (fprintf ppf "%s: ")) alias
          name
          pp_arguments arguments
          pp_directives directives
          pp_selection_set selection_set
    | FragmentSpread {name; directives} ->
        fprintf ppf "... %s %a" name pp_directives directives
    | InlineFragment {type_condition; directives; selection_set} ->
        fprintf ppf "...%a %a %a"
          (pp_option (fprintf ppf " on %s")) type_condition
          pp_directives directives
          pp_selection_set selection_set

  and pp_selection_set ppf =
    omit_empty_list (fprintf ppf "{@[<hv 2>@,%a@]}" (list pp_selection))

  let rec pp_typ ppf = function
    | NamedType t -> fprintf ppf "%s" t
    | ListType t -> fprintf ppf "[%a]" pp_typ t
    | NonNullType t -> fprintf ppf "%a!" pp_typ t

  let pp_variable_definition ppf {default_value; name; typ} =
    fprintf ppf "$%s : %a%a"
      name
      pp_typ typ
      (pp_option (fprintf ppf " = %a" pp_value)) default_value

  let pp_variables ppf =
   omit_empty_list (fprintf ppf "(%a)" (comma_separated pp_variable_definition))

  let optype_to_string = function
    | Query -> "query"
    | Mutation -> "mutation"
    | Subscription -> "subscription"

  let pp_operation ppf op =
    match op.name with
    | None ->
        pp_selection_set ppf op.selection_set
    | Some name ->
        fprintf ppf "%s %s%a%a %a"
          (optype_to_string op.optype)
          name
          pp_variables op.variable_definitions
          pp_directives op.directives
          pp_selection_set op.selection_set

  let pp_fragment ppf {name; type_condition; directives; selection_set} =
    fprintf ppf "fragment %s on %s %a %a" name type_condition
      pp_directives directives
      pp_selection_set selection_set

  let pp_definition ppf = function
    | Operation op -> pp_operation ppf op
    | Fragment f -> pp_fragment ppf f

  let pp_document = list pp_definition
end

let pp_document = Pp.pp_document
