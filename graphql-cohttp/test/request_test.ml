open Lwt.Infix

module Graphql_cohttp_lwt = Graphql_cohttp.Make (Graphql_lwt.Schema) (Cohttp_lwt_unix.IO) (Cohttp_lwt.Body)

let schema = Graphql_lwt.Schema.(schema [
  field "hello"
    ~typ:(non_null string)
    ~args:Arg.[
      arg "name" ~typ:string
    ]
    ~resolve:(fun _ () -> function
      | None -> "world"
      | Some name -> name
    )
])

let response_check = Alcotest.of_pp Cohttp.Response.pp_hum

let check_body actual expected =
  Cohttp_lwt.Body.to_string actual >|= fun body ->
  Alcotest.(check string) "body" body expected

let check_response_action ~rsp ~rsp_body actual =
  match actual with
  | `Expert _ ->
      Alcotest.fail "Got expert, expected response"
  | `Response (rsp', body') ->
    Alcotest.check response_check "response" rsp' rsp;
    check_body body' rsp_body

let default_uri = Uri.of_string "/graphql"

let json_content_type = Cohttp.Header.init_with "Content-Type" "application/json"
let graphql_content_type = Cohttp.Header.init_with "Content-Type" "application/graphql"

let default_response_body =
  Yojson.Basic.to_string (`Assoc [
    "data", `Assoc [
      "hello", `String "world"
    ]
  ])

let test_case ~req ~req_body ~rsp ~rsp_body =
  Lwt_main.run begin
    Graphql_cohttp_lwt.execute_request schema () req req_body >>=
    check_response_action
      ~rsp
      ~rsp_body
  end

let suite = [
  ("POST with empty body", `Quick, fun () ->
    test_case
      ~req:(Cohttp.Request.make ~meth:`POST default_uri)
      ~req_body:Cohttp_lwt.Body.empty
      ~rsp:(Cohttp.Response.make ~status:`Bad_request ())
      ~rsp_body:"Must provide query string"
  );
  ("POST with json body", `Quick, fun () ->
    let req_body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string (`Assoc [
      "query", `String "{ hello }"
    ])) in
    test_case
      ~req:(Cohttp.Request.make ~meth:`POST ~headers:json_content_type default_uri)
      ~req_body
      ~rsp:(Cohttp.Response.make ~status:`OK ())
      ~rsp_body:default_response_body
  );
  ("POST with graphql body", `Quick, fun () ->
    let req_body = Cohttp_lwt.Body.of_string "{ hello }" in
    test_case
      ~req:(Cohttp.Request.make ~meth:`POST ~headers:graphql_content_type default_uri)
      ~req_body
      ~rsp:(Cohttp.Response.make ~status:`OK ())
      ~rsp_body:default_response_body
  );
  ("GET with empty query string", `Quick, fun () ->
    test_case
      ~req:(Cohttp.Request.make ~meth:`GET default_uri)
      ~req_body:Cohttp_lwt.Body.empty
      ~rsp:(Cohttp.Response.make ~status:`Bad_request ())
      ~rsp_body:"Must provide query string"
  );
  ("GET with query", `Quick, fun () ->
    let query = "{ hello }" in
    let query = Some ["query", [query]] in
    let uri = Uri.with_uri ~query default_uri in
    test_case
      ~req:(Cohttp.Request.make ~meth:`GET uri)
      ~req_body:Cohttp_lwt.Body.empty
      ~rsp:(Cohttp.Response.make ~status:`OK ())
      ~rsp_body:default_response_body
  );
  ("operation name in JSON body", `Quick, fun () ->
    let req_body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string (`Assoc [
      "query", `String "query A { hello(name: \"world\") } query B { hello(name: \"fail\") }";
      "operationName", `String "A"
    ])) in
    test_case
      ~req:(Cohttp.Request.make ~meth:`POST ~headers:json_content_type default_uri)
      ~req_body
      ~rsp:(Cohttp.Response.make ~status:`OK ())
      ~rsp_body:default_response_body
  );
  ("operation name in query string", `Quick, fun () ->
    let req_body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string (`Assoc [
      "query", `String "query A { hello(name: \"world\") } query B { hello(name: \"fail\") }"
    ])) in
    let query = Some ["operationName", ["A"]] in
    let uri = Uri.with_uri ~query default_uri in
    test_case
      ~req:(Cohttp.Request.make ~meth:`POST ~headers:json_content_type uri)
      ~req_body
      ~rsp:(Cohttp.Response.make ~status:`OK ())
      ~rsp_body:default_response_body
  );
  ("variables in JSON body", `Quick, fun () ->
    let req_body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string (`Assoc [
      "query", `String "query A($name: String!) { hello(name: $name) }";
      "variables", `Assoc [
        "name", `String "world"
      ]
    ])) in
    test_case
      ~req:(Cohttp.Request.make ~meth:`POST ~headers:json_content_type default_uri)
      ~req_body
      ~rsp:(Cohttp.Response.make ~status:`OK ())
      ~rsp_body:default_response_body
  );
  ("variables in query string", `Quick, fun () ->
    let req_body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string (`Assoc [
      "query", `String "query A($name: String!) { hello(name: $name) }";
    ])) in
    let query =
      Some [
        "operationName", ["A"];
        "variables", ["{\"name\":\"world\"}"];
      ]
    in
    let uri = Uri.with_uri ~query default_uri in
    test_case
      ~req:(Cohttp.Request.make ~meth:`POST ~headers:json_content_type uri)
      ~req_body
      ~rsp:(Cohttp.Response.make ~status:`OK ())
      ~rsp_body:default_response_body
  );
]
