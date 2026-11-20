let schema = Graphql_lwt.Schema.(schema [])

(* Example handshake from RFC 6455, section 1.3. *)
let key = "dGhlIHNhbXBsZSBub25jZQ=="

let expected_accept = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="

(* Upgrade request with the [Connection] header [connection], the [Upgrade]
   header [upgrade], and [headers]. *)
let upgrade_request ?(connection = "Upgrade") ?(upgrade = "websocket") headers
    =
  let headers =
    Cohttp.Header.of_list
      ([ ("Connection", connection); ("Upgrade", upgrade) ] @ headers)
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

(* Checks that [Handshake.is_upgrade] is [expected] for a request with the
   headers [Connection: connection] and [Upgrade: upgrade]. *)
let check_is_upgrade expected connection upgrade =
  Alcotest.(check bool)
    (connection ^ " / " ^ upgrade)
    expected
    (Graphql_websocket.Handshake.is_upgrade
       (Cohttp.Header.of_list
          [ ("Connection", connection); ("Upgrade", upgrade) ]))

module Ws = Graphql_websocket.Connection.Make (Cohttp_lwt_unix.Server.IO)

(* Subprotocol selected by [Ws.upgrade_connection ?protocols] when the client
   offers [offered]. *)
let selected_protocol ?protocols offered =
  let req =
    upgrade_request
      [ ("Sec-WebSocket-Key", key); ("Sec-WebSocket-Protocol", offered) ]
  in
  match Ws.upgrade_connection ?protocols req (fun _ -> Lwt.return_unit) with
  | `Expert (rsp, _) ->
      Cohttp.Header.get (Cohttp.Response.headers rsp) "Sec-WebSocket-Protocol"

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
    ( "upgrade to websocket with a Connection token list",
      `Quick,
      fun () ->
        (* As sent by Firefox, with an Upgrade value in another case. *)
        let req =
          upgrade_request ~connection:"keep-alive, Upgrade"
            ~upgrade:"WebSocket"
            [ ("Sec-WebSocket-Key", key) ]
        in
        match upgrade req with
        | `Response _ -> Alcotest.fail "Got response, expected expert"
        | `Expert (rsp, _) ->
            check_header rsp "Sec-WebSocket-Accept" (Some expected_accept) );
    ( "detection of upgrade requests",
      `Quick,
      fun () ->
        (* As sent by nginx with its usual configuration. *)
        check_is_upgrade true "upgrade" "websocket";
        check_is_upgrade true "Upgrade, keep-alive" "WEBSOCKET";
        check_is_upgrade false "keep-alive" "websocket";
        check_is_upgrade false "Upgrade" "h2c";
        check_is_upgrade false "Upgrade" "websockets" );
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
    ( "upgrade to websocket with several subprotocols",
      `Quick,
      fun () ->
        let req =
          upgrade_request
            [
              ("Sec-WebSocket-Key", key);
              ("Sec-WebSocket-Protocol", "graphql-transport-ws, graphql-ws");
              ("Sec-WebSocket-Protocol", "foo");
            ]
        in
        match upgrade req with
        | `Response _ -> Alcotest.fail "Got response, expected expert"
        | `Expert (rsp, _) ->
            check_header rsp "Sec-WebSocket-Protocol" (Some "graphql-ws") );
    ( "upgrade to websocket with an unsupported subprotocol",
      `Quick,
      fun () ->
        let req =
          upgrade_request
            [
              ("Sec-WebSocket-Key", key);
              ("Sec-WebSocket-Protocol", "graphql-transport-ws");
            ]
        in
        match upgrade req with
        | `Response _ -> Alcotest.fail "Got response, expected expert"
        | `Expert (rsp, _) -> check_header rsp "Sec-WebSocket-Protocol" None );
    ( "subprotocol in the order of the client",
      `Quick,
      fun () ->
        Alcotest.(check (option string))
          "selected" (Some "a")
          (selected_protocol ~protocols:[ "b"; "a" ] "c, a, b") );
    ( "no subprotocol by default",
      `Quick,
      fun () ->
        Alcotest.(check (option string))
          "selected" None
          (selected_protocol "graphql-ws") );
    ( "upgrade to websocket without key",
      `Quick,
      fun () ->
        match upgrade (upgrade_request []) with
        | `Expert _ -> Alcotest.fail "Got expert, expected response"
        | `Response (rsp, _) ->
            Alcotest.(check int)
              "status" 400
              (Cohttp.Code.code_of_status (Cohttp.Response.status rsp)) );
  ]
