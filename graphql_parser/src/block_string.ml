let get_block_string_indentation (s : string) : int =
  let is_first_line = ref true in
  let is_empty_line = ref true in
  let indent = ref 0 in
  let common_indent = ref None in
  let i = ref 0 in
  while !i < String.length s do
    ( match s.[!i] with
    | ('\r' as c) | ('\n' as c) ->
        if c == '\r' && try s.[!i + 1] == '\n' with _ -> false then incr i;
        is_first_line := false;
        is_empty_line := true;
        indent := 0
    | '\t' | ' ' -> incr indent
    | _ ->
        if
          !is_empty_line && (not !is_first_line)
          &&
          match !common_indent with
          | None -> true
          | Some common_indent -> !indent < common_indent
        then common_indent := Some !indent );
    incr i
  done;
  match !common_indent with None -> 0 | Some num -> num

let split_lines (s : string) : string list =
  let lines = ref [] in
  let b = Buffer.create 32 in
  let i = ref 0 in
  let n = String.length s in
  while !i < n do
    ( match s.[!i] with
    | ('\r' as c) | ('\n' as c) ->
        if c == '\r' && try s.[!i + 1] == '\n' with _ -> false then incr i;
        lines := Buffer.contents b :: !lines;
        Buffer.clear b
    | c -> Buffer.add_char b c );
    incr i
  done;
  lines := Buffer.contents b :: !lines;
  List.rev !lines

exception NotBlank

let is_blank (s : string) : bool =
  try
    String.iter (function ' ' | '\t' -> () | _ -> raise NotBlank) s;
    true
  with NotBlank -> false

let dedent_block_string_value (s : string) : string =
  let lines = split_lines s |> Array.of_list in
  let start_line = ref 0 in
  while !start_line < Array.length lines && is_blank lines.(!start_line) do
    incr start_line
  done;
  let end_line = ref (Array.length lines) in
  while !end_line > !start_line && is_blank lines.(!end_line - 1) do
    decr end_line
  done;
  let common_indent = get_block_string_indentation s in
  let i = ref !start_line in
  let b = Buffer.create 32 in
  while !i < !end_line do
    let line = lines.(!i) in
    Buffer.add_string b
      ( match (!i, common_indent == 0) with
      | 0, _ | _, true -> line
      | _, false ->
          let start = min (String.length line) common_indent in
          String.sub line start (String.length line - start) );
    incr i;
    match !i != !end_line with true -> Buffer.add_char b '\n' | false -> ()
  done;
  Buffer.contents b
