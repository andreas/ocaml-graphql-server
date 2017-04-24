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
]
