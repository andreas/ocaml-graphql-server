open Test_helper

let suite : (string * [>`Quick] * (unit -> unit)) list = [
  ("nullable", `Quick, fun () ->
    let _, mk_variables, _ = [%graphql {|
        query Foo($int: Int, $string: String, $bool: Boolean, $float: Float, $id: ID, $enum: COLOR) {
          non_nullable_int
        }
    |}] in
    let variables = mk_variables id ~int:1 ~string:"2" ~bool:true ~float:12.3 ~id:"42" ~enum:`RED () in
    let expected = `Assoc [
      "int", `Int 1;
      "string", `String "2";
      "bool", `Bool true;
      "float", `Float 12.3;
      "id", `String "42";
      "enum", `String "RED"
    ] in
    Alcotest.(check yojson) "nullable" expected variables;
  );
  ("non-nullable", `Quick, fun () ->
    let _, mk_variables, _ = [%graphql {|
        query Foo($int: Int!, $string: String!, $bool: Boolean!, $float: Float!, $id: ID!, $enum: COLOR!) {
          non_nullable_int
        }
    |}] in
    let variables = mk_variables id ~int:1 ~string:"2" ~bool:true ~float:12.3 ~id:"42" ~enum:`RED () in
    let expected = `Assoc [
      "int", `Int 1;
      "string", `String "2";
      "bool", `Bool true;
      "float", `Float 12.3;
      "id", `String "42";
      "enum", `String "RED"
    ] in
    Alcotest.(check yojson) "non_nullable" expected variables;
  );
  ("list", `Quick, fun () ->
    let _, mk_variables, _ = [%graphql {|
        query Foo($list: [Int]) {
          non_nullable_int
        }
    |}] in
    let variables = mk_variables id ~list:[Some 1; None] () in
    let expected = `Assoc [
      "list", `List [`Int 1; `Null]
    ] in
    Alcotest.(check yojson) "list" expected variables;
  );
]