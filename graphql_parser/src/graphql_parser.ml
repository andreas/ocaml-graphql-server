open Result

include Graphql_ast

let position_to_string lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Format.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse query =
  let lexbuf = Lexing.from_string query in
  try
    Ok (Graphql_grammar.doc Graphql_lexer.token lexbuf)
  with
  | Graphql_grammar.Error ->
      let pos = position_to_string lexbuf in
      let err = Format.sprintf "%s: syntax error\n" pos in
      Error err
