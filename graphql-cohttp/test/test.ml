let () =
  Alcotest.run "graphql-cohttp"
    [ ("request", Request_test.suite); ("websocket", Websocket_test.suite) ]
