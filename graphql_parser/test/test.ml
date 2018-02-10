let () =
  Alcotest.run "graphql_parser" [
    "parse", Parser_test.suite;
  ]
