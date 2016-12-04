open Graphql
open Test_common

let echo : 'a. unit -> unit -> 'a -> 'a = fun _ _ x -> x

let echo_field name field_typ arg_typ = Schema.(
      field ~name
            ~typ:field_typ
            ~args:Arg.[
              arg "x" ~typ:arg_typ
            ]
            ~resolve:echo
)

type colors = Red | Green | Blue
let color_enum     = Schema.enum ~name:"color" ~values:[Red, "RED"; Green, "GREEN"; Blue, "BLUE"]
let color_enum_arg = Schema.Arg.enum ~name:"color" ~values:["RED", Red; "GREEN", Green; "BLUE", Blue]

let person_arg = Schema.Arg.(obj ~name:"person" ~fields:Arg.[
    arg "title" ~typ:string;
    arg "first_name" ~typ:(non_null string);
    arg "last_name" ~typ:(non_null string);
  ]
  ~coerce:(fun title first last -> (title, first, last))
)

let echo_schema =
  Schema.(schema ~fields:[
      echo_field "string" string Arg.string;
      echo_field "float" float Arg.float;
      echo_field "int" int Arg.int;
      echo_field "bool" bool Arg.bool;
      echo_field "enum" color_enum color_enum_arg;
      echo_field "id" guid Arg.guid;
      echo_field "bool_list" (list bool) Arg.(list bool);
      field ~name:"input_obj"
            ~typ:(non_null string)
            ~args:Arg.[
              arg "x" ~typ:(non_null person_arg)
            ]
            ~resolve:(fun () () (title, first, last) -> first ^ " " ^ last)
  ])

let suite : (string * [>`Quick] * (unit -> unit)) list = [
  ("string argument", `Quick, fun () ->
    test_query echo_schema () "{ string(x: \"foo\") }" "{\"data\":{\"string\":\"foo\"}}"
  );
  ("float argument", `Quick, fun () ->
    test_query echo_schema () "{ float(x: 42.5) }" "{\"data\":{\"float\":42.5}}"
  );
  ("int argument", `Quick, fun () ->
    test_query echo_schema () "{ int(x: 42) }" "{\"data\":{\"int\":42}}"
  );
  ("bool argument", `Quick, fun () ->
    test_query echo_schema () "{ bool(x: false) }" "{\"data\":{\"bool\":false}}"
  );
  ("enum argument", `Quick, fun () ->
    test_query echo_schema () "{ enum(x: RED) }" "{\"data\":{\"enum\":\"RED\"}}"
  );
  ("list argument", `Quick, fun () ->
    test_query echo_schema () "{ bool_list(x: [false, true]) }" "{\"data\":{\"bool_list\":[false,true]}}"
  );
  ("input object argument", `Quick, fun () ->
    test_query echo_schema () "{ input_obj(x: {title: \"Mr\", first_name: \"John\", last_name: \"Doe\"}) }" "{\"data\":{\"input_obj\":\"John Doe\"}}"
  );
  ("null for optional argument", `Quick, fun () ->
    test_query echo_schema () "{ string(x: null) }" "{\"data\":{\"string\":null}}"
  );
  ("null for required argument", `Quick, fun () ->
    test_query echo_schema () "{ input_obj(x: null) }" "{\"errors\":[{\"message\":\"Missing required argument\"}]}"
  );
  ("missing optional argument", `Quick, fun () ->
    test_query echo_schema () "{ string }" "{\"data\":{\"string\":null}}"
  );
  ("missing required argument", `Quick, fun () ->
    test_query echo_schema () "{ input_obj }" "{\"errors\":[{\"message\":\"Missing required argument\"}]}"
  );
  ("input coercion: single value to list", `Quick, fun () ->
    test_query echo_schema () "{ bool_list(x: false) }" "{\"data\":{\"bool_list\":[false]}}"
  );
  ("input coercion: int to float", `Quick, fun () ->
    test_query echo_schema () "{ float(x: 42) }" "{\"data\":{\"float\":42.0}}"
  );
  ("input coercion: int to ID", `Quick, fun () ->
    test_query echo_schema () "{ id(x: 42) }" "{\"data\":{\"id\":\"42\"}}"
  );
  ("input coercion: string to ID", `Quick, fun () ->
    test_query echo_schema () "{ id(x: \"42\") }" "{\"data\":{\"id\":\"42\"}}"
  );
]