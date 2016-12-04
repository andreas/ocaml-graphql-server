let test_query schema ctx query expected =
  match Graphql.Parser.parse query with
  | Error err -> failwith err
  | Ok doc ->
      let result = Graphql.execute schema ctx doc in
      let result' = Yojson.Basic.to_string result in
      Alcotest.(check string) "invalid execution result" expected result'