let test_query = Test_common.test_query Test_schema.schema ()

let suite =
  [
    ( "query",
      `Quick,
      fun () ->
        let query = "{ users { id } }" in
        test_query query
          (`Assoc
            [
              ( "data",
                `Assoc
                  [
                    ( "users",
                      `List
                        [
                          `Assoc [ ("id", `Int 1) ];
                          `Assoc [ ("id", `Int 2) ];
                        ] );
                  ] );
            ]) );
    ( "mutation",
      `Quick,
      fun () ->
        let query =
          "mutation { add_user(name: \"Charlie\", role: \"user\") { name } }"
        in
        test_query query
          (`Assoc
            [
              ( "data",
                `Assoc
                  [
                    ( "add_user",
                      `List
                        [
                          `Assoc [ ("name", `String "Alice") ];
                          `Assoc [ ("name", `String "Bob") ];
                          `Assoc [ ("name", `String "Charlie") ];
                        ] );
                  ] );
            ]) );
    ( "__typename",
      `Quick,
      fun () ->
        let query = "{ __typename }" in
        test_query query
          (`Assoc [ ("data", `Assoc [ ("__typename", `String "query") ]) ]) );
    ( "select operation (no operations)",
      `Quick,
      fun () ->
        let query = "fragment x on y { z }" in
        test_query query
          (`Assoc
            [
              ( "errors",
                `List [ `Assoc [ ("message", `String "No operation found") ] ]
              );
            ]) );
    ( "select operation (one operation, no operation name)",
      `Quick,
      fun () ->
        let query = "query a { a: __typename }" in
        test_query query
          (`Assoc [ ("data", `Assoc [ ("a", `String "query") ]) ]) );
    ( "select operation (one operation, matching operation name)",
      `Quick,
      fun () ->
        let query = "query a { a: __typename }" in
        test_query query ~operation_name:"a"
          (`Assoc [ ("data", `Assoc [ ("a", `String "query") ]) ]) );
    ( "select operation (one operation, missing operation name)",
      `Quick,
      fun () ->
        let query = "query a { a: __typename }" in
        test_query query ~operation_name:"b"
          (`Assoc
            [
              ( "errors",
                `List [ `Assoc [ ("message", `String "Operation not found") ] ]
              );
            ]) );
    ( "select operation (multiple operations, no operation name)",
      `Quick,
      fun () ->
        let query = "query a { a: __typename } query b { b: __typename }" in
        test_query query
          (`Assoc
            [
              ( "errors",
                `List
                  [ `Assoc [ ("message", `String "Operation name required") ] ]
              );
            ]) );
    ( "select operation (multiple operations, matching operation name)",
      `Quick,
      fun () ->
        let query = "query a { a: __typename } query b { b: __typename }" in
        test_query query ~operation_name:"b"
          (`Assoc [ ("data", `Assoc [ ("b", `String "query") ]) ]) );
    ( "select operation (multiple operations, missing operation name)",
      `Quick,
      fun () ->
        let query = "query a { a: __typename } query b { b: __typename }" in
        test_query query ~operation_name:"c"
          (`Assoc
            [
              ( "errors",
                `List [ `Assoc [ ("message", `String "Operation not found") ] ]
              );
            ]) );
    ( "undefined field on query root",
      `Quick,
      fun () ->
        let query = "{ foo { bar } }" in
        test_query query
          (`Assoc
            [
              ( "errors",
                `List
                  [
                    `Assoc
                      [
                        ( "message",
                          `String "Field 'foo' is not defined on type 'query'"
                        );
                      ];
                  ] );
            ]) );
    ( "undefined field on object type",
      `Quick,
      fun () ->
        let query = "{ users { id foo } }" in
        test_query query
          (`Assoc
            [
              ( "errors",
                `List
                  [
                    `Assoc
                      [
                        ( "message",
                          `String "Field 'foo' is not defined on type 'user'"
                        );
                      ];
                  ] );
            ]) );
    ( "fragments cannot form cycles",
      `Quick,
      fun () ->
        let query =
          "\n\
          \      fragment F1 on Foo {\n\
          \        ... on Bar {\n\
          \          baz {\n\
          \            ... F2\n\
          \          }\n\
          \        }\n\
          \      }\n\n\
          \      fragment F2 on Qux {\n\
          \        ... F1\n\
          \      }\n\n\
          \      {\n\
          \        ... F1\n\
          \      }\n\
          \    "
        in
        test_query query
          (`Assoc
            [
              ( "errors",
                `List
                  [
                    `Assoc
                      [
                        ("message", `String "Fragment cycle detected: F1, F2");
                      ];
                  ] );
            ]) );
    ( "fragments combine nested fields",
      `Quick,
      fun () ->
        let query =
          "\n\
          \      query Q {\n\
          \        users {\n\
          \          role\n\
          \        }\n\
          \        ...F1\n\
          \      }\n\
          \      fragment F1 on query {\n\
          \        users {\n\
          \          name\n\
          \        }\n\
          \      }\n\
          \    "
        in
        test_query query
          (`Assoc
            [
              ( "data",
                `Assoc
                  [
                    ( "users",
                      `List
                        [
                          `Assoc
                            [
                              ("role", `String "admin");
                              ("name", `String "Alice");
                            ];
                          `Assoc
                            [
                              ("role", `String "user");
                              ("name", `String "Bob");
                            ];
                          `Assoc
                            [
                              ("role", `String "user");
                              ("name", `String "Charlie");
                            ];
                        ] );
                  ] );
            ]) );
    ( "introspection query should be accepted",
      `Quick,
      fun () ->
        let query =
          "\n\
          \      query IntrospectionQuery {\n\
          \        __schema {\n\
          \          queryType { name }\n\
          \          mutationType { name }\n\
          \          subscriptionType { name }\n\
          \          types {\n\
          \            ...FullType\n\
          \          }\n\
          \          directives {\n\
          \            name\n\
          \            description\n\
          \            locations\n\
          \            args {\n\
          \              ...InputValue\n\
          \            }\n\
          \          }\n\
          \        }\n\
          \      }\n\n\
          \      fragment FullType on __Type {\n\
          \        kind\n\
          \        name\n\
          \        description\n\
          \        fields(includeDeprecated: true) {\n\
          \          name\n\
          \          description\n\
          \          args {\n\
          \            ...InputValue\n\
          \          }\n\
          \          type {\n\
          \            ...TypeRef\n\
          \          }\n\
          \          isDeprecated\n\
          \          deprecationReason\n\
          \        }\n\
          \        inputFields {\n\
          \          ...InputValue\n\
          \        }\n\
          \        interfaces {\n\
          \          ...TypeRef\n\
          \        }\n\
          \        enumValues(includeDeprecated: true) {\n\
          \          name\n\
          \          description\n\
          \          isDeprecated\n\
          \          deprecationReason\n\
          \        }\n\
          \        possibleTypes {\n\
          \          ...TypeRef\n\
          \        }\n\
          \      }\n\n\
          \      fragment InputValue on __InputValue {\n\
          \        name\n\
          \        description\n\
          \        type { ...TypeRef }\n\
          \        defaultValue\n\
          \      }\n\n\
          \      fragment TypeRef on __Type {\n\
          \        kind\n\
          \        name\n\
          \        ofType {\n\
          \          kind\n\
          \          name\n\
          \          ofType {\n\
          \            kind\n\
          \            name\n\
          \            ofType {\n\
          \              kind\n\
          \              name\n\
          \              ofType {\n\
          \                kind\n\
          \                name\n\
          \                ofType {\n\
          \                  kind\n\
          \                  name\n\
          \                  ofType {\n\
          \                    kind\n\
          \                    name\n\
          \                    ofType {\n\
          \                      kind\n\
          \                      name\n\
          \                    }\n\
          \                  }\n\
          \                }\n\
          \              }\n\
          \            }\n\
          \          }\n\
          \        }\n\
          \      }\n\
          \    "
        in
        match Graphql_parser.parse query with
        | Error err -> failwith err
        | Ok doc -> (
            match Graphql.Schema.execute Test_schema.schema () doc with
            | Ok _ -> ()
            | Error err -> failwith (Yojson.Basic.pretty_to_string err) ) );
    ( "subscription",
      `Quick,
      fun () ->
        let query = "subscription { subscribe_to_user { id name } }" in
        test_query query
          (`List
            [
              `Assoc
                [
                  ( "data",
                    `Assoc
                      [
                        ( "subscribe_to_user",
                          `Assoc [ ("id", `Int 1); ("name", `String "Alice") ]
                        );
                      ] );
                ];
            ]) );
    ( "subscription returns an error",
      `Quick,
      fun () ->
        let query =
          "subscription { subscribe_to_user(error: true) { id name } }"
        in
        test_query query
          (`Assoc
            [
              ( "errors",
                `List
                  [
                    `Assoc
                      [
                        ("message", `String "stream error");
                        ("path", `List [ `String "subscribe_to_user" ]);
                      ];
                  ] );
              ("data", `Null);
            ]) );
    ( "subscriptions: exn inside the stream",
      `Quick,
      fun () ->
        let query =
          "subscription { subscribe_to_user(raise: true) { id name } }"
        in
        test_query query (`String "caught stream exn") );
    ( "subscription returns more than one value",
      `Quick,
      fun () ->
        let query =
          "subscription { subscribe_to_user(first: 2) { id name } }"
        in
        test_query query
          (`List
            [
              `Assoc
                [
                  ( "data",
                    `Assoc
                      [
                        ( "subscribe_to_user",
                          `Assoc [ ("id", `Int 1); ("name", `String "Alice") ]
                        );
                      ] );
                ];
              `Assoc
                [
                  ( "data",
                    `Assoc
                      [
                        ( "subscribe_to_user",
                          `Assoc [ ("id", `Int 2); ("name", `String "Bob") ] );
                      ] );
                ];
            ]) );
  ]
