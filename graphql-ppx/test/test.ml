let () =
  Alcotest.run "graphql-ppx" [
    "parsing", Parsing_test.suite;
    "variables", Variables_test.suite;
    "integration", Integration_test.suite;
  ]