open Test_common

let suite = [
  ("query", `Quick, fun () ->
    test_query Test_schema.schema () "{ users { id } }" "{\"data\":{\"users\":[{\"id\":1},{\"id\":2}]}}"
  );
  ("mutation", `Quick, fun () ->
    test_query Test_schema.schema () "mutation { add_user(name: \"Charlie\", role: \"user\") { name } }" "{\"data\":{\"add_user\":[{\"name\":\"Alice\"},{\"name\":\"Bob\"},{\"name\":\"Charlie\"}]}}"
  );
]