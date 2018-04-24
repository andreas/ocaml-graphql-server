open Graphql
open Test_common

let suite = [
  ("nullable error", `Quick, fun () ->
    let schema = Schema.(schema [
      io_field "nullable"
        ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _ () -> Error "boom")
    ]) in
    let query = "{ nullable }" in
    test_query schema () query (`Assoc [
      "data", `Assoc [
        "nullable", `Null
      ];
      "errors", `List [
        `Assoc [
          "message", `String "boom"
        ]
      ]
    ])
  );
  ("non-nullable error", `Quick, fun () ->
    let schema = Schema.(schema [
      io_field "non_nullable"
        ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _ () -> Error "boom")
    ]) in
    let query = "{ non_nullable }" in
    test_query schema () query (`Assoc [
      "data", `Null;
      "errors", `List [
        `Assoc [
          "message", `String "boom"
        ]
      ]
    ])
  );
  ("nested nullable error", `Quick, fun () ->
    let obj_with_non_nullable_field = Schema.(obj "obj"
        ~fields:(fun _ -> [
          io_field "non_nullable"
            ~typ:(non_null int)
            ~args:Arg.[]
            ~resolve:(fun _ () -> Error "boom")
        ]))
    in
    let schema = Schema.(schema [
      field "nullable"
        ~typ:obj_with_non_nullable_field
        ~args:Arg.[]
        ~resolve:(fun _ () -> Some ())
      ])
    in
    let query = "{ nullable { non_nullable } }" in
    test_query schema () query (`Assoc [
      "data", `Assoc [
        "nullable", `Null
      ];
      "errors", `List [
        `Assoc [
          "message", `String "boom"
        ]
      ]
    ])
  );
]
