open Test_helper

let executable (query, variables, of_json) =
  fun ~response ->
    variables (fun vars ->
      vars, of_json response
    )

let query_type =
  let query_formatter formatter t = Format.pp_print_text formatter (string_of_int t#non_nullable_int) in
  let query_eq a b = a#non_nullable_int = b#non_nullable_int in
  Alcotest.testable query_formatter query_eq

let suite : (string * [>`Quick] * (unit -> unit)) list = [
  ("simple query with argument", `Quick, fun () ->
    let exec_query = executable [%graphql {|
        query MyQuery($int: Int) {
          non_nullable_int
        }
    |}] in
    let expected = `Assoc ["int", `Int 1], object method non_nullable_int = 42 end in
    let response = `Assoc ["data", `Assoc [
      "non_nullable_int", `Int 42
    ]] in
    Alcotest.(check (pair yojson query_type)) "response" expected (exec_query ~response ~int:1 ());
  );
]