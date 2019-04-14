let () =
  Alcotest.run "graphql-cohttp" [
    "request", Request_test.suite;
  ]
