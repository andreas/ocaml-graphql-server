open Test_common

let suite = [
  ("execution", `Quick, fun () ->
    test_query Test_schema.schema () "{ users { id } }" "{\"data\":{\"users\":[{\"id\":1},{\"id\":2}]}}"
  );
]