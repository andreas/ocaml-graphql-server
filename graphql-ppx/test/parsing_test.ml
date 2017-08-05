let color_enum = Alcotest.of_pp (fun formatter t ->
  let txt = match t with
  | `RED -> "RED"
  | `GREEN -> "GREEN"
  | `BLUE -> "BLUE"
  in Format.pp_print_text formatter txt
)

let suite : (string * [>`Quick] * (unit -> unit)) list = [
  ("nullable primitives", `Quick, fun () ->
    let _, _, of_json = [%graphql {|
        query {
            nullable_int
            nullable_string
            nullable_float
            nullable_bool
            nullable_id
            nullable_enum
        }
    |}] in
    let response = `Assoc ["data", `Assoc [
        "nullable_int", `Int 42;
        "nullable_string", `String "42";
        "nullable_float", `Float 42.0;
        "nullable_bool", `Bool true;
        "nullable_id", `String "42";
        "nullable_enum", `String "RED";
    ]] in
    let parsed = of_json response in
    Alcotest.(check (option int)) "nullable int" (Some 42) parsed#nullable_int;
    Alcotest.(check (option string)) "nullable string" (Some "42") parsed#nullable_string;
    Alcotest.(check (option float)) "nullable float" (Some 42.0) parsed#nullable_float;
    Alcotest.(check (option bool)) "nullable bool" (Some true) parsed#nullable_bool;
    Alcotest.(check (option string)) "nullable ID" (Some "42") parsed#nullable_id;
    Alcotest.(check (option color_enum)) "nullable enum" (Some `RED) parsed#nullable_enum;
  );
  ("non-nullable primitives", `Quick, fun () ->
    let _, _, of_json = [%graphql {|
        query {
            non_nullable_int
            non_nullable_string
            non_nullable_float
            non_nullable_bool
            non_nullable_id
            non_nullable_enum
        }
    |}] in
    let response = `Assoc ["data", `Assoc [
        "non_nullable_int", `Int 42;
        "non_nullable_string", `String "42";
        "non_nullable_float", `Float 42.0;
        "non_nullable_bool", `Bool true;
        "non_nullable_id", `String "42";
        "non_nullable_enum", `String "GREEN";
    ]] in
    let parsed = of_json response in
    Alcotest.(check int) "non_nullable int" 42 parsed#non_nullable_int;
    Alcotest.(check string) "non_nullable string" "42" parsed#non_nullable_string;
    Alcotest.(check float) "non_nullable float" 42.0 parsed#non_nullable_float;
    Alcotest.(check bool) "non_nullable bool" true parsed#non_nullable_bool;
    Alcotest.(check string) "non_nullable ID" "42" parsed#non_nullable_id;
    Alcotest.(check color_enum) "nullable enum" `GREEN parsed#non_nullable_enum;
  );
  ("list of primitives", `Quick, fun () ->
    let _, _, of_json = [%graphql {|
        query {
            list_int
            list_string
            list_float
            list_bool
            list_id
            list_enum
        }
    |}] in
    let response = `Assoc ["data", `Assoc [
        "list_int", `List [`Int 1; `Int 2];
        "list_string", `List [`String "1"; `String "2"];
        "list_float", `List [`Float 1.1; `Float 2.2];
        "list_bool", `List [`Bool false; `Bool true];
        "list_id", `List [`String "1"; `Int 2];
        "list_enum", `List [`String "BLUE"; `String "RED"];
    ]] in
    let parsed = of_json response in
    Alcotest.(check (option (list (option int)))) "list int" (Some [Some 1; Some 2]) parsed#list_int;
    Alcotest.(check (option (list (option string)))) "list string" (Some [Some "1"; Some "2"]) parsed#list_string;
    Alcotest.(check (option (list (option float)))) "list float" (Some [Some 1.1; Some 2.2]) parsed#list_float;
    Alcotest.(check (option (list (option bool)))) "list bool" (Some [Some false; Some true]) parsed#list_bool;
    Alcotest.(check (option (list (option string)))) "list ID" (Some [Some "1"; Some "2"]) parsed#list_id;
    Alcotest.(check (option (list (option color_enum)))) "list enum" (Some [Some `BLUE; Some `RED]) parsed#list_enum;
  );
  ("object", `Quick, fun () ->
    let _, _, of_json = [%graphql {|
        query {
            person {
                id
                age
                name
                nickname
                net_worth
                favorite_color
                friends {
                    id
                    favorite_color
                }
            }
        }
    |}] in
    let response = `Assoc ["data", `Assoc [
        "person", `Assoc [
            "id", `Int 42;
            "age", `Int 30;
            "name", `String "John Doe";
            "nickname", `Null;
            "net_worth", `Float 1_000.0;
            "favorite_color", `String "BLUE";
            "friends", `List [
                `Assoc [
                    "id", `String "9";
                    "favorite_color", `Null;
                ];
                `Null
            ]
        ]
    ]] in
    let parsed = of_json response in
    Alcotest.(check string) "id" "42" parsed#person#id;
    Alcotest.(check int) "age" 30 parsed#person#age;
    Alcotest.(check string) "name" "John Doe" parsed#person#name;
    Alcotest.(check (option string)) "nickname" None parsed#person#nickname;
    Alcotest.(check (option float)) "net_worth" (Some 1_000.0) parsed#person#net_worth;
    Alcotest.(check (option color_enum)) "favorite_color" (Some `BLUE) parsed#person#favorite_color;
    let Some friend0 = List.nth parsed#person#friends 0 in
    Alcotest.(check string) "friend id" "9" friend0#id;
    Alcotest.(check (option color_enum)) "friend color" None friend0#favorite_color;
    let friend1 = List.nth parsed#person#friends 1 in
    Alcotest.(check (option pass)) "null friend" None friend1;
  );
  ("aliasing", `Quick, fun () ->
    let _, _, of_json = [%graphql {|
        query {
            alias: non_nullable_int
        }
    |}] in
    let response = `Assoc ["data", `Assoc [
        "alias", `Int 42;
    ]] in
    let parsed = of_json response in
    Alcotest.(check int) "alias" 42 parsed#alias;
  );
]