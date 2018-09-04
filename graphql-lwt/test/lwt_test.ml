open Lwt

let yojson = (module struct
  type t = Yojson.Basic.json

  let pp formatter t =
    Format.pp_print_text formatter (Yojson.Basic.pretty_to_string t)

  let equal = (=)
end : Alcotest.TESTABLE with type t = Yojson.Basic.json)

let test_query schema ctx query expected =
  Lwt_main.run begin
    match Graphql_parser.parse query with
    | Error err -> Lwt.fail_with err
    | Ok doc ->
      Graphql_lwt.Schema.execute schema ctx doc >>= (function
      | Ok (`Response data) -> Lwt.return data
      | Ok (`Stream (stream, _)) ->
          Lwt_stream.to_list stream >|= fun lst ->
            `List (
              List.fold_right (fun x acc -> match x with
                | Ok data -> data :: acc
                | _ -> acc) lst [])
      | Error err -> Lwt.return err)
      >|= fun result ->
        Alcotest.check yojson "invalid execution result" expected result
  end

let schema = Graphql_lwt.Schema.(schema [
      field "direct_string"
        ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun () () -> "foo")
      ;
      io_field "io_int"
        ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun () () -> Lwt.return (Ok 42))
    ]
  ~subscriptions:[
    subscription_field "int_stream"
      ~typ:(non_null int)
      ~args:Arg.[]
      ~resolve:(fun () ->
        let stream = Lwt_stream.of_list [1; 2; 3] in
        let destroy = (fun () -> ()) in
        Lwt_result.return (stream, destroy))
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
  ("subscription", `Quick, fun () ->
    test_query schema () "subscription { int_stream }" (`List [
      `Assoc [
        "data", `Assoc [
          "int_stream", `Int 1
        ]
      ];
      `Assoc [
        "data", `Assoc [
          "int_stream", `Int 2
        ]
      ];
      `Assoc [
        "data", `Assoc [
          "int_stream", `Int 3
        ]
      ]
    ])
  )
]

let () = Alcotest.run "graphql-server" ["lwt", suite]
