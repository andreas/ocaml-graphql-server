let yojson = (module struct
  type t = Yojson.Basic.json

  let pp formatter t =
    Format.pp_print_text formatter (Yojson.Basic.pretty_to_string t)

  let equal = (=)
end : Alcotest.TESTABLE with type t = Yojson.Basic.json)

let test_query schema ctx ?variables ?operation_name query expected =
  match Graphql_parser.parse query with
  | Error err -> failwith err
  | Ok doc ->
      let result = match Graphql.Schema.execute schema ctx ?variables ?operation_name doc with
      | Ok data -> data
      | Error err -> err
      in
      Alcotest.check yojson "invalid execution result" expected result
