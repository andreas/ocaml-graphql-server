open Graphql
open Async_kernel
open Async_unix

let yojson = (module struct
  type t = Yojson.Basic.json

  let pp formatter t =
    Format.pp_print_text formatter (Yojson.Basic.pretty_to_string t)

  let equal = (=)
end : Alcotest.TESTABLE with type t = Yojson.Basic.json)

let test_query schema ctx query expected =
  Thread_safe.block_on_async_exn begin fun () ->
    match Graphql_parser.parse query with
    | Error err -> failwith err
    | Ok doc ->
      Graphql_async.Schema.execute schema ctx doc >>| fun result ->
      let result' = match result with
      | Ok data -> data
      | Error err -> err
      in
      Alcotest.check yojson "invalid execution result" expected result'
  end

let schema = Graphql_async.Schema.(schema [
      field "direct_string"
        ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun () () -> "foo")
      ;
      io_field "io_int"
        ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun () () -> Deferred.return (Ok 42))
    ]
)

let suite = [
  ("execution", `Quick, fun () ->
    test_query schema () "{ direct_string io_int  }" (`Assoc [
      "data", `Assoc [
        "direct_string", `String "foo";
        "io_int", `Int 42
      ]
    ])
  );
]

let () = Alcotest.run "graphql-server" ["async", suite]
