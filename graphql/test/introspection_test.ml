open Graphql

let suite = [
  ("not deprecated", `Quick, fun () ->
    let schema = Schema.(schema [
      field "not-deprecated"
        ~deprecated:NotDeprecated
        ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _ _ -> Some "")
    ]) in
    Test_common.test_query schema () "{ __schema { queryType { fields { isDeprecated deprecationReason } } } }" "{\"data\":{\"__schema\":{\"queryType\":{\"fields\":[{\"isDeprecated\":false,\"deprecationReason\":null}]}}}}"
  );
  ("default deprecation", `Quick, fun () ->
    let schema = Schema.(schema [
      field "default"
        ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _ _ -> Some "")
    ]) in
    Test_common.test_query schema () "{ __schema { queryType { fields { isDeprecated deprecationReason } } } }" "{\"data\":{\"__schema\":{\"queryType\":{\"fields\":[{\"isDeprecated\":false,\"deprecationReason\":null}]}}}}"
  );
  ("deprecated-without-reason", `Quick, fun () ->
    let schema = Schema.(schema [
      field "deprecated-without-reason"
        ~deprecated:(Deprecated None)
        ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _ _ -> Some "")
    ]) in
    Test_common.test_query schema () "{ __schema { queryType { fields { isDeprecated deprecationReason } } } }" "{\"data\":{\"__schema\":{\"queryType\":{\"fields\":[{\"isDeprecated\":true,\"deprecationReason\":null}]}}}}"
  );
  ("deprecated with reason", `Quick, fun () ->
    let schema = Schema.(schema [
      field "deprecated-with-reason"
        ~deprecated:(Deprecated (Some "deprecation reason"))
        ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _ _ -> Some "")
    ]) in
    Test_common.test_query schema () "{ __schema { queryType { fields { isDeprecated deprecationReason } } } }" "{\"data\":{\"__schema\":{\"queryType\":{\"fields\":[{\"isDeprecated\":true,\"deprecationReason\":\"deprecation reason\"}]}}}}"
  );
]
