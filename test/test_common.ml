let test_query schema ctx query expected =
  match Graphql_parser.parse query with
  | Error err -> failwith err
  | Ok doc ->
      let result = Graphql.Schema.execute schema ctx doc in
      let result' = Yojson.Basic.to_string result in
      Alcotest.(check string) "invalid execution result" expected result'