open Test_common

let suite = [
  ("query", `Quick, fun () ->
    let query = "{ users { id } }" in
    test_query Test_schema.schema () query "{\"data\":{\"users\":[{\"id\":1},{\"id\":2}]}}"
  );
  ("mutation", `Quick, fun () ->
    let query = "mutation { add_user(name: \"Charlie\", role: \"user\") { name } }" in
    test_query Test_schema.schema () query "{\"data\":{\"add_user\":[{\"name\":\"Alice\"},{\"name\":\"Bob\"},{\"name\":\"Charlie\"}]}}"
  );
  ("__typename", `Quick, fun () ->
    let query = "{ __typename }" in
    test_query Test_schema.schema () query "{\"data\":{\"__typename\":\"query\"}}"
  );
  ("select operation (no operations)", `Quick, fun () ->
    let query = "fragment x on y { z }" in
    test_query Test_schema.schema () query "{\"errors\":[{\"message\":\"No operation found\"}]}"
  );
  ("select operation (one operation, no operation name)", `Quick, fun () ->
    let query = "query a { a: __typename }" in
    test_query Test_schema.schema () query "{\"data\":{\"a\":\"query\"}}"
  );
  ("select operation (one operation, matching operation name)", `Quick, fun () ->
    let query = "query a { a: __typename }" in
    test_query Test_schema.schema () query ~operation_name:"a" "{\"data\":{\"a\":\"query\"}}"
  );
  ("select operation (one operation, missing operation name)", `Quick, fun () ->
    let query = "query a { a: __typename }" in
    test_query Test_schema.schema () query ~operation_name:"b" "{\"errors\":[{\"message\":\"Operation not found\"}]}"
  );
  ("select operation (multiple operations, no operation name)", `Quick, fun () ->
    let query = "query a { a: __typename } query b { b: __typename }" in
    test_query Test_schema.schema () query "{\"errors\":[{\"message\":\"Operation name required\"}]}"
  );
  ("select operation (multiple operations, matching operation name)", `Quick, fun () ->
    let query = "query a { a: __typename } query b { b: __typename }" in
    test_query Test_schema.schema () query ~operation_name:"b" "{\"data\":{\"b\":\"query\"}}"
  );
  ("select operation (multiple operations, missing operation name)", `Quick, fun () ->
    let query = "query a { a: __typename } query b { b: __typename }" in
    test_query Test_schema.schema () query ~operation_name:"c" "{\"errors\":[{\"message\":\"Operation not found\"}]}"
  );
]
