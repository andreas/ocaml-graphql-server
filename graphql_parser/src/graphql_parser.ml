module Ast = struct
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
end
include Ast

module Parser = struct
  open Angstrom

  let optional p = option None (lift (fun x -> Some x) p)
  let optional_list p = option [] p
  let lift5 f a b c d e = lift4 f a b c d <*> e

  let ignored = scan_state `Whitespace (fun state c ->
    match state with
    | `Comment ->
        if c = '\n' then Some `Whitespace else Some `Comment
    | `Whitespace ->
        match c with
        | ' ' | ',' | '\t' | '\n' -> Some `Whitespace
        | '#' -> Some `Comment
        | _ -> None
  ) >>| fun _ -> ()

  let ( *~>) a b = (a *> ignored) *> b
  let ( <~*) a b = (a <* ignored) <* b

  let lift2' f a b = lift2 f (a <* ignored) b
  let lift3' f a b c = lift3 f (a <* ignored) (b <* ignored) c
  let lift4' f a b c d = lift4 f (a <* ignored) (b <* ignored) (c <* ignored) d
  let lift5' f a b c d e = lift5 f (a <* ignored) (b <* ignored) (c <* ignored) (d <* ignored) e

  let ellipsis = string "..."
  let lparen = char '('
  let rparen = char ')'
  let lbrace = char '{'
  let rbrace = char '}'
  let lbrack = char '['
  let rbrack = char ']'
  let bang   = char '!'
  let colon  = char ':'
  let equal  = char '='
  let dollar = char '$'
  let at     = char '@'
  let dot    = char '.'
  let dash   = char '-'
  let quote  = char '"'

  let is_name_char =
    function | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_'  -> true | _ -> false
  let name = take_while1 is_name_char

  let is_number_char =
    function | '0' .. '9' | 'e' | 'E' | '.' | '-' | '+' -> true | _ -> false
  let number_chars = take_while1 is_number_char

  let string_buf = Buffer.create 8

  let string_chars = scan_state `Unescaped (fun state c ->
      match state with
      | `Escaped ->
          Buffer.add_char string_buf c;
          Some `Unescaped
      | `Unescaped ->
          match c with
          | '\\' -> Some `Escaped
          | '"' -> None
          | _ ->
              Buffer.add_char string_buf c;
              Some `Unescaped
    ) >>= fun _ ->
    let s = Buffer.contents string_buf in
    Buffer.clear string_buf;
    return s

  let null = string "null" *> return `Null

  let variable = lift (fun n -> `Variable n) (dollar *> name)

  let string_value = lift (fun s -> `String s) (quote *> string_chars <* quote)

  let boolean_value = string "true"  *> return (`Bool true) <|>
                      string "false" *> return (`Bool false)

  let number_value = lift (fun n ->
    try
      `Int (int_of_string n)
    with Failure _ ->
      `Float (float_of_string n)) number_chars

  let enum_value = name >>= function
    | "true"
    | "false"
    | "null" as n -> fail (Format.sprintf "Invalid enum value: %s" n)
    | n -> return (`Enum n)

  let value_parser value_types = fix (fun value' ->
    let list_value = lbrack *~> rbrack *> return (`List []) <|>
                     lift (fun l -> `List l) (lbrack *~> sep_by1 ignored value' <~* rbrack)
    and object_field = lift2' (fun name value -> name, value) (name <~* colon) value'
    in
    let object_value = lbrace *~> rbrace *> return (`Assoc []) <|>
                       lift (fun p -> `Assoc p) (lbrace *~> sep_by1 ignored object_field <~* rbrace)
    in
      List.fold_left (<|>) (list_value <|> object_value) value_types
    )

  let value : value Angstrom.t = value_parser [
    null;
    number_value;
    string_value;
    boolean_value;
    enum_value;
    variable
  ]

  let const_value : const_value Angstrom.t = value_parser [
    null;
    number_value;
    string_value;
    boolean_value;
    enum_value
  ]

  let argument = lift2' (fun name value -> name, value)
                   (name <~* colon) value

  let arguments = lparen *~> sep_by ignored argument <~* rparen

  let directive = lift2' (fun name arguments -> {name; arguments})
                    (at *> name) (optional_list arguments)

  let directives = sep_by ignored directive

  let typ = fix (fun typ' ->
    let named_type = lift (fun n -> NamedType n) name
    and list_type = lift (fun t -> ListType t) (lbrack *~> typ' <~* rbrack)
    and non_null = option false (bang *> return true)
    in
      lift2' (fun t non_null -> if non_null then NonNullType t else t)
        (named_type <|> list_type) non_null
  )

  let variable_definition = lift3' (fun name typ default_value -> {
    name;
    typ;
    default_value;
  }) (dollar *~> name <~* colon) typ (optional (equal *~> const_value))

  let variable_definitions = lparen *~> many variable_definition <~* rparen

  let alias = name <~* colon

  let fragment_name = name >>= function
    | "on" -> fail "Invalid fragment name `on`"
    | n    -> return n

  let type_condition = string "on" *~> name

  let selection_set = fix (fun selection_set' ->
    let field =
      lift5' (fun alias name arguments directives selection_set -> Field {
        alias;
        name;
        arguments;
        directives;
        selection_set;
      }) (optional alias) name (optional_list arguments) (optional_list directives) (optional_list selection_set')
    and fragment_spread =
      lift2' (fun name directives -> FragmentSpread {name; directives})
      (ellipsis *~> fragment_name) (optional_list directives)
    and inline_fragment =
      lift3' (fun type_condition directives selection_set -> InlineFragment {
        type_condition;
        directives;
        selection_set;
      })
      (ellipsis *~> optional type_condition) (optional_list directives) selection_set'
    in let selection =
      field <|>
      fragment_spread <|>
      inline_fragment
    in lbrace *~> sep_by1 ignored selection <~* rbrace
  )

  let optype = (string "query"        *> return Query) <|>
               (string "mutation"     *> return Mutation) <|>
               (string "subscription" *> return Subscription)

  let operation_definition =
    lift5' (fun optype name variable_definitions directives selection_set -> Operation {
      optype;
      name;
      variable_definitions;
      directives;
      selection_set;
    }) (option Query optype) (optional name) (optional_list variable_definitions) (optional_list directives) selection_set

  let fragment_definition =
    lift4' (fun name type_condition directives selection_set -> Fragment {
      name;
      type_condition;
      directives;
      selection_set;
    })
    (string "fragment" *~> fragment_name) type_condition (optional_list directives) selection_set

  let definition = operation_definition <|> fragment_definition

  let document = many1 (ignored *> definition)
end

module Pp = struct
  let comma = Fmt.(const string ",")
  let colon = Fmt.(const string ":")

  let rec pp_value fmt = function
    | `Null -> Fmt.string fmt "null"
    | `Int n -> Fmt.int fmt n
    | `Float f -> Fmt.float fmt f
    | `String s ->
        let escaped = Str.(global_replace (regexp "\"") "\\\"") s in
        Fmt.(quote string) fmt escaped
    | `Bool b -> Fmt.bool fmt b
    | `Enum e -> Fmt.string fmt e
    | `Variable s -> Fmt.fmt "$%s" fmt s
    | `List l -> Fmt.(brackets (list ~sep:comma pp_value)) fmt l
    | `Assoc props -> Fmt.(braces (list ~sep:comma (pair ~sep:colon string pp_value))) fmt props

  let omit_empty_list t =
    fun fmt -> function
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

let parse query = Angstrom.parse_string Parser.document query
