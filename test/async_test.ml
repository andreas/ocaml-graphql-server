open Graphql
open Async_kernel.Std
open Async_unix.Std

let test_query schema ctx query expected =
  Thread_safe.block_on_async_exn begin fun () ->
    match Graphql_parser.parse query with
    | Error err -> failwith err
    | Ok doc ->
        Graphql_async.Schema.execute schema ctx doc >>| fun result ->
        let result' = Yojson.Basic.to_string result in
        Alcotest.(check string) "invalid execution result" expected result'
  end

let schema = Graphql_async.Schema.(schema
    ~fields:[
      field "direct_string"
        ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun () () -> "foo")
      ;
      io_field "io_int"
        ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun () () -> Deferred.return 42)
    ]
)

let suite = [
  ("execution", `Quick, fun () ->
    test_query schema () "{ direct_string io_int  }" "{\"data\":{\"direct_string\":\"foo\",\"io_int\":42}}"
  );
]