open Sexplib.Std

let read_all path =
  let file = open_in path in
  try
      really_input_string file (in_channel_length file)
  with exn ->
    close_in file;
    raise exn

let test_query_file filename () =
  let query    = read_all ("data/"^filename^".graphql") in
  let expected = read_all ("data/"^filename^".sexp") |> String.trim in
  match Graphql_parser.parse query with
  | Ok doc ->
      let sexp = Graphql_parser.sexp_of_document doc |> Sexplib.Sexp.to_string_hum in
      Alcotest.(check string) "invalid parse result" expected sexp
  | Error err -> failwith (Format.sprintf "Failed to parse %s: %s" filename err)

let suite = [
  "introspection", `Quick, test_query_file "introspection";
  "kitchen_sink",  `Quick, test_query_file "kitchen_sink";
  "escaped_string", `Quick, test_query_file "escaped_string";
]
