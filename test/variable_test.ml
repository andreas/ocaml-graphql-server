let test_query variables = Test_common.test_query Echo_schema.schema () ~variables

let suite : (string * [>`Quick] * (unit -> unit)) list = [
  ("string variable", `Quick, fun () ->
    test_query ["x", `String "foo bar baz"] "{ string(x: $x) }" "{\"data\":{\"string\":\"foo bar baz\"}}"
  );
  ("float variable", `Quick, fun () ->
    test_query ["x", `Float 42.5] "{ float(x: $x) }" "{\"data\":{\"float\":42.5}}"
  );
  ("int variable", `Quick, fun () ->
    test_query ["x", `Int 42] "{ int(x: $x) }" "{\"data\":{\"int\":42}}"
  );
  ("bool variable", `Quick, fun () ->
    test_query ["x", `Bool false] "{ bool(x: $x) }" "{\"data\":{\"bool\":false}}"
  );
  ("enum variable", `Quick, fun () ->
    test_query ["x", `Enum "RED"] "{ enum(x: $x) }" "{\"data\":{\"enum\":\"RED\"}}"
  );
  ("list variable", `Quick, fun () ->
    test_query ["x", `List [`Bool true; `Bool false]] "{ bool_list(x: [false, true]) }" "{\"data\":{\"bool_list\":[false,true]}}"
  );
  ("input object variable", `Quick, fun () ->
    let obj = `Assoc [
      "title", `String "Mr";
      "first_name", `String "John";
      "last_name", `String "Doe";
    ] in
    test_query ["x", obj] "{ input_obj(x: {title: \"Mr\", first_name: \"John\", last_name: \"Doe\"}) }" "{\"data\":{\"input_obj\":\"John Doe\"}}"
  );
  ("null for optional variable", `Quick, fun () ->
    test_query ["x", `Null]"{ string(x: $x) }" "{\"data\":{\"string\":null}}"
  );
  ("null for required variable", `Quick, fun () ->
    test_query ["x", `Null] "{ input_obj(x: $x) }" "{\"errors\":[{\"message\":\"Missing required argument\"}]}"
  );
  ("variable coercion: single value to list", `Quick, fun () ->
    test_query ["x", `Bool false] "{ bool_list(x: $x) }" "{\"data\":{\"bool_list\":[false]}}"
  );
  ("variable coercion: int to float", `Quick, fun () ->
    test_query ["x", `Int 42] "{ float(x: $x) }" "{\"data\":{\"float\":42.0}}"
  );
  ("variable coercion: int to ID", `Quick, fun () ->
    test_query ["x", `Int 42] "{ id(x: $x) }" "{\"data\":{\"id\":\"42\"}}"
  );
  ("variable coercion: string to ID", `Quick, fun () ->
    test_query ["x", `String "42"] "{ id(x: $x) }" "{\"data\":{\"id\":\"42\"}}"
  );
]
