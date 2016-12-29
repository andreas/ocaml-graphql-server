open Graphql
open Lwt

let test_query schema ctx query expected =
  Lwt_main.run begin
    match Graphql_parser.parse query with
    | Error err -> Lwt.fail_with err
    | Ok doc ->
      Graphql_lwt.Schema.execute schema ctx doc >|= fun result ->
      let result' = match result with
      | Ok data -> data
      | Error err -> err
      in
      Alcotest.(check string) "invalid execution result" expected (Yojson.Basic.to_string result')
  end

let schema = Graphql_lwt.Schema.(schema
    ~fields:[
      field "direct_string"
        ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun () () -> "foo")
      ;
      io_field "io_int"
        ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun () () -> Lwt.return 42)
    ]
)

let suite = [
  ("execution", `Quick, fun () ->
    test_query schema () "{ direct_string io_int  }" "{\"data\":{\"direct_string\":\"foo\",\"io_int\":42}}"
  );
]