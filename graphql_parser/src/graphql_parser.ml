include Ast

let string_of_pos (pos : Lexing.position) =
  Format.sprintf "Line %d col %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse s =
  let lexbuf = Lexing.from_string s in
  try Ok (Parser.doc Lexer.token lexbuf) with
  | Parser.Error ->
    let pos = lexbuf.lex_start_p in
    Error (Format.sprintf "%s: Syntax error" (string_of_pos pos))
  | Lexer.Error msg ->
    let pos = lexbuf.lex_curr_p in
    Error (Format.sprintf "%s: %s" (string_of_pos pos) msg)
