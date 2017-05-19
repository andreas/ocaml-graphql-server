let test_query = Test_common.test_query Echo_schema.schema ()

let suite : (string * [>`Quick] * (unit -> unit)) list = [
  ("string argument", `Quick, fun () ->
    test_query "{ string(x: \"foo bar baz\") }" "{\"data\":{\"string\":\"foo bar baz\"}}"
  );
  ("float argument", `Quick, fun () ->
    test_query "{ float(x: 42.5) }" "{\"data\":{\"float\":42.5}}"
  );
  ("int argument", `Quick, fun () ->
    test_query "{ int(x: 42) }" "{\"data\":{\"int\":42}}"
  );
  ("bool argument", `Quick, fun () ->
    test_query "{ bool(x: false) }" "{\"data\":{\"bool\":false}}"
  );
  ("enum argument", `Quick, fun () ->
    test_query "{ enum(x: RED) }" "{\"data\":{\"enum\":\"RED\"}}"
  );
  ("list argument", `Quick, fun () ->
    test_query "{ bool_list(x: [false, true]) }" "{\"data\":{\"bool_list\":[false,true]}}"
  );
  ("input object argument", `Quick, fun () ->
    test_query "{ input_obj(x: {title: \"Mr\", first_name: \"John\", last_name: \"Doe\"}) }" "{\"data\":{\"input_obj\":\"John Doe\"}}"
  );
  ("null for optional argument", `Quick, fun () ->
    test_query "{ string(x: null) }" "{\"data\":{\"string\":null}}"
  );
  ("null for required argument", `Quick, fun () ->
    test_query "{ input_obj(x: null) }" "{\"errors\":[{\"message\":\"Missing required argument\"}]}"
  );
  ("missing optional argument", `Quick, fun () ->
    test_query "{ string }" "{\"data\":{\"string\":null}}"
  );
  ("missing required argument", `Quick, fun () ->
    test_query "{ input_obj }" "{\"errors\":[{\"message\":\"Missing required argument\"}]}"
  );
  ("input coercion: single value to list", `Quick, fun () ->
    test_query "{ bool_list(x: false) }" "{\"data\":{\"bool_list\":[false]}}"
  );
  ("input coercion: int to float", `Quick, fun () ->
    test_query "{ float(x: 42) }" "{\"data\":{\"float\":42.0}}"
  );
  ("input coercion: int to ID", `Quick, fun () ->
    test_query "{ id(x: 42) }" "{\"data\":{\"id\":\"42\"}}"
  );
  ("input coercion: string to ID", `Quick, fun () ->
    test_query "{ id(x: \"42\") }" "{\"data\":{\"id\":\"42\"}}"
  );
  ("default arguments", `Quick, fun () ->
    test_query "{ sum_defaults }" "{\"data\":{\"sum_defaults\":45}}"
  )
]
