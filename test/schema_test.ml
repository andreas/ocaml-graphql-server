let test_query schema ctx query expected =
  match Graphql.Parser.parse query with
  | Ok doc ->
      let result = Graphql.execute schema ctx doc |> Yojson.Basic.to_string in
      Alcotest.(check string) "invalid execution result" expected result
  | Error err ->
      failwith (Format.sprintf "Failed to parse query (%s): %s" err query)

let test_simple () =
  test_query Test_schema.schema () "{ users { id } }" "{\"users\":[{\"id\":1},{\"id\":2}]}"

let suite = [
  "simple", `Quick, test_simple;
]
