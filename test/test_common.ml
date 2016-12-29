let test_query schema ctx query expected =
  match Graphql_parser.parse query with
  | Error err -> failwith err
  | Ok doc ->
      let result = match Graphql.Schema.execute schema ctx doc with
      | Ok data -> data
      | Error err -> err
      in
      Alcotest.(check string) "invalid execution result" expected (Yojson.Basic.to_string result)