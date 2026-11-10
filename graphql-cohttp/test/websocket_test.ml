let schema = Graphql_lwt.Schema.(schema [])

(* Example handshake from RFC 6455, section 1.3. *)
let key = "dGhlIHNhbXBsZSBub25jZQ=="

let expected_accept = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="

let upgrade_request headers =
  let headers =
    Cohttp.Header.of_list
      ([ ("Connection", "Upgrade"); ("Upgrade", "websocket") ] @ headers)
  in
  Cohttp.Request.make ~meth:`GET ~headers (Uri.of_string "/graphql")

let upgrade req =
  Lwt_main.run
    (Graphql_cohttp_lwt.make_callback
       (fun _req -> ())
       schema () req Cohttp_lwt.Body.empty)

let check_header rsp name expected =
  Alcotest.(check (option string))
    name expected
    (Cohttp.Header.get (Cohttp.Response.headers rsp) name)

let suite =
  [
    ( "upgrade to websocket",
      `Quick,
      fun () ->
        match upgrade (upgrade_request [ ("Sec-WebSocket-Key", key) ]) with
        | `Response _ -> Alcotest.fail "Got response, expected expert"
        | `Expert (rsp, _) ->
            Alcotest.(check int)
              "status" 101
              (Cohttp.Code.code_of_status (Cohttp.Response.status rsp));
            check_header rsp "Upgrade" (Some "websocket");
            check_header rsp "Connection" (Some "Upgrade");
            check_header rsp "Sec-WebSocket-Accept" (Some expected_accept);
            check_header rsp "Sec-WebSocket-Protocol" None );
    ( "upgrade to websocket with subprotocol",
      `Quick,
      fun () ->
        let req =
          upgrade_request
            [
              ("Sec-WebSocket-Key", key);
              ("Sec-WebSocket-Protocol", "graphql-ws");
            ]
        in
        match upgrade req with
        | `Response _ -> Alcotest.fail "Got response, expected expert"
        | `Expert (rsp, _) ->
            check_header rsp "Sec-WebSocket-Protocol" (Some "graphql-ws") );
  ]
