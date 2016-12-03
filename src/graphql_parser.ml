open Sexplib.Std
open Angstrom

(* Language type definitions *)

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

and key_value = {
    name : string;
    value : value
  }
  [@@deriving sexp]

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

(* Parser combinators *)

let comment =
  char '#' *> skip_while (function | '\n' -> false | _ -> true )

let is_ignored_char =
  function | ' ' | ',' | '\n' -> true | _ -> false

let ignored = skip_while is_ignored_char <|> comment

let lex p = ignored *> p
let char c = lex (Angstrom.char c)
let string s = lex (Angstrom.string s)

let is_name_char =
  function | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_'  -> true | _ -> false

let is_int_char =
  function | '0' .. '9' | '-' -> true | _ -> false

let is_float_char =
  function | '0' .. '9' | 'e' | 'E' | '.' | '-' | '+' -> true | _ -> false

let optional p = option None (lift (fun x -> Some x) p)
let optional_list p = option [] p
let lift5 f a b c d e = lift4 f a b c d >>= fun f -> e >>| f

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

let string_chars = ignored *> take_while1 is_name_char
let int_chars    = ignored *> take_while1 is_int_char
let float_chars  = ignored *> take_while1 is_float_char
let name         = ignored *> take_while1 is_name_char 

let null = string "null" *> return Null
let variable = lift (fun n -> Variable n) (dollar *> name)
let int_value = lift (fun i -> Int (int_of_string i)) int_chars
let float_value = lift (fun f -> Float (float_of_string f)) float_chars
let string_value = lift (fun s -> String s) (quote *> string_chars <* quote)
let boolean_value = string "true"  *> return (Boolean true) <|>
                    string "false" *> return (Boolean false)
let enum_value = name >>= function
  | "true"
  | "false"
  | "null" as n -> fail (Format.sprintf "Invalid enum value: %s" n)
  | n -> return (Enum n)

let value = fix (fun value' ->
  let list_value = lbrack *> rbrack *> return (List []) <|>
                   lift (fun l -> List l) (lbrack *> many value' <* rbrack)
  and object_field = lift2 (fun name value -> { name; value }) (name <* colon) value'
  in
  let object_value = lbrace *> rbrace *> return (Object []) <|>
                     lift (fun p -> Object p) (lbrace *> many object_field <* rbrace)
  in
    null <|>
    variable <|>
    int_value <|>
    float_value <|>
    string_value <|>
    boolean_value <|>
    enum_value <|>
    list_value <|>
    object_value
)

let argument = lift2 (fun name value -> {name; value})
               (name <* colon) value

let arguments = lparen *> many argument <* rparen

let directive = lift2 (fun name arguments -> {name; arguments})
                (at *> name) (optional_list arguments)

let directives = many directive

let typ = fix (fun typ' ->
  let named_type = lift (fun n -> NamedType n) name
  and list_type = lift (fun t -> ListType t) (lbrack *> typ' <* rbrack)
  in let non_null_type = lift (fun t -> NonNullType t) ((named_type <|> list_type) <* bang)
  in
    named_type <|>
    list_type <|>
    non_null_type
)

let variable_definition = lift3 (fun name typ default_value -> {
    name;
    typ;
    default_value;
  }) (dollar *> name <* colon) typ (optional (equal *> value))

let variable_definitions = lparen *> many variable_definition <* rparen

let fragment_name = name >>= function
  | "on" -> fail "Invalid fragment name `on`"
  | n    -> return n

let type_condition = string "on" *> name

let alias = name <* colon

let selection_set = fix (fun selection_set' ->
  let field =
    lift5 (fun alias name arguments directives selection_set -> Field {
      alias;
      name;
      arguments;
      directives;
      selection_set;
    }) (optional alias) name (optional_list arguments) (optional_list directives) (optional_list selection_set')
  and fragment_spread =
    lift2 (fun name directives -> FragmentSpread {name; directives})
    (ellipsis *> fragment_name) (optional_list directives)
  and inline_fragment =
    lift3 (fun type_condition directives selection_set -> InlineFragment {
      type_condition;
      directives;
      selection_set;
    })
    (ellipsis *> optional type_condition) (optional_list directives) selection_set'
  in let selection =
    field <|>
    fragment_spread <|>
    inline_fragment
  in lbrace *> many1 selection <* rbrace
)

let optype = (string "query"        *> return Query) <|>
             (string "mutation"     *> return Mutation) <|>
             (string "subscription" *> return Subscription)

let operation_definition =
  lift5 (fun optype name variable_definitions directives selection_set -> Operation {
    optype;
    name;
    variable_definitions;
    directives;
    selection_set;
  }) (option Query optype) (optional name) (optional_list variable_definitions) (optional_list directives) selection_set

let fragment_definition =
  lift4 (fun name type_condition directives selection_set -> Fragment {
    name;
    type_condition;
    directives;
    selection_set;
  })
  (string "fragment" *> fragment_name) type_condition (optional_list directives) selection_set

let definition = operation_definition <|> fragment_definition

let document = many1 definition

let parse query = Angstrom.parse_only document (`String query)
