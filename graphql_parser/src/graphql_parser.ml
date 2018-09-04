open Sexplib.Std
open Angstrom

(* Language type definitions *)

type primitive_value = [
  | `Null
  | `Int of int
  | `Float of float
  | `String of string
  | `Bool of bool
  | `Enum of string
] [@@deriving sexp]

type const_value = [
  | primitive_value
  | `List of const_value list
  | `Assoc of (string * const_value) list
] [@@deriving sexp]

type value = [
  | primitive_value
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

(* Parser combinators *)

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
let quote  = char '"'

let is_name_char =
  function | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_'  -> true | _ -> false
let name = take_while1 is_name_char

let is_number_char =
  function | '0' .. '9' | 'e' | 'E' | '.' | '-' | '+' -> true | _ -> false
let number_chars = take_while1 is_number_char

let string_buf = Buffer.create 0x1000

let string_chars = scan_state `Unescaped (fun state c ->
    let add_char c =
      Buffer.add_char string_buf c;
      Some `Unescaped
    in
    match state with
    | `Error _ -> None
    | `Escaped ->
        begin match c with
        | '"'  -> add_char '"';
        | '\\' -> add_char '\\';
        | '/'  -> add_char '/';
        | 'b'  -> add_char '\b';
        | 'f'  -> add_char '\012';
        | 'n'  -> add_char '\n';
        | 't'  -> add_char '\t';
        | 'r'  -> add_char '\r';
        | _    -> Some (`Error "Invalid escape sequence")
        end
    | `Unescaped ->
        begin match c with
        | '\\' -> Some `Escaped
        | '"'  -> None
        | c ->
            if c >= '\032' then
              add_char c
            else
              let err = Printf.sprintf "Unexpected character '%c'" c in
              Some (`Error err)
        end
  ) >>= function
  | `Error err ->
      Buffer.clear string_buf;
      fail err
  | `Escaped ->
      Buffer.clear string_buf;
      fail "Unterminated string"
  | `Unescaped ->
      let s = Buffer.contents string_buf in
      Buffer.clear string_buf;
      return s

let null = string "null" *> return `Null

let variable = lift (fun n -> `Variable n) (dollar *> name)

let string_value = lift (fun s -> `String s) (quote *> string_chars <* quote)

let boolean_value = string "true"  *> return (`Bool true) <|>
                    string "false" *> return (`Bool false)

let number_value = number_chars >>= fun n ->
  try
    return (`Int (int_of_string n))
  with Failure _ ->
    try
      return (`Float (float_of_string n))
    with Failure _ ->
      fail (Format.sprintf "Invalid number value: %s" n)

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

let parse query = Angstrom.parse_string document query
