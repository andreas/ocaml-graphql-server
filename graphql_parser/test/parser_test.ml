let graphql_query = Alcotest.testable Fmt.string (fun a b ->
  let graphql_ignored = Str.regexp "[ ,\t\r\n]" in
  let strip = Str.global_replace graphql_ignored "" in
  (strip a) = (strip b)
)

let test_query query =
  match Graphql_parser.parse query with
  | Ok doc ->
      let query' = Fmt.to_to_string Graphql_parser.pp_document doc in
      Alcotest.(check graphql_query) "Parse result" query query'
  | Error err ->
      Alcotest.failf "Failed to parse %s: %s" query err

let test_introspection_query () =
  test_query
    "query IntrospectionQuery {
      __schema {
        queryType { name }
        mutationType { name }
        subscriptionType { name }
        types {
          ...FullType
        }
        directives {
          name
          description
          args {
            ...InputValue
          }
          onOperation
          onFragment
          onField
        }
      }
    }

    fragment FullType on __Type {
      kind
      name
      description
      fields(includeDeprecated: true) {
        name
        description
        args {
          ...InputValue
        }
        type {
          ...TypeRef
        }
        isDeprecated
        deprecationReason
      }
      inputFields {
        ...InputValue
      }
      interfaces {
        ...TypeRef
      }
      enumValues(includeDeprecated: true) {
        name
        description
        isDeprecated
        deprecationReason
      }
      possibleTypes {
        ...TypeRef
      }
    }

    fragment InputValue on __InputValue {
      name
      description
      type { ...TypeRef }
      defaultValue
    }

    fragment TypeRef on __Type {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
          }
        }
      }
    }"

let test_kitchen_sink () =
  test_query
    "query queryName($foo: ComplexType, $site: Site = MOBILE) {
      whoever123is: node(id: [123, 456]) {
        id ,
        ... on User @defer {
          field2 {
            id ,
            alias: field1(first:10, after:$foo,) @include(if: $foo) {
              id,
              ...frag
            }
          }
        }
        ... @skip(unless: $foo) {
          id
        }
        ... {
          id
        }
      }
    }

    mutation likeStory {
      like(story: 123) @defer {
        story {
          id
        }
      }
    }

    subscription StoryLikeSubscription($input: StoryLikeSubscribeInput) {
      storyLikeSubscribe(input: $input) {
        story {
          likers {
            count
          }
          likeSentence {
            text
          }
        }
      }
    }

    fragment frag on Friend {
      foo(size: $size, bar: $b, obj: {key: \"value\"})
    }

    {
      unnamed(truthy: true, falsey: false, nullish: null),
      query
    }"

let test_variables () =
  test_query
    "query Named($a: String, $b: Float) {
      x
    }

    query List($a: [Bool], $b: [[Int]]) {
      x
    }

    query NonNull($a: ID!, $b: [Foo]!, $c: [Foo!], $d: [Foo!]!) {
      x
    }"

let test_default_values () =
  test_query
    "query DefaultValues(
        $a: Int = 1,
        $b: [String] = [\"a\"],
        $c: Obj = {x: {y: true}, z: RED},
        $d: [Obj]! = [{x: {y: null}, z: BLUE}]
      ) {
      x
    }"

let test_keywords () =
  test_query
    "query Keywords(
      $fragment: Int,
      $false: Int,
      $mutation: Int,
      $null: Int,
      $on: Int,
      $query: Int,
      $subscription: Int,
      $true: Int
    ) {
      fragment
      false
      mutation
      null
      on
      query
      subscription
      true
    }

    fragment fragment on Foo {
      a
    }

    fragment false on Foo {
      a
    }

    fragment mutation on Foo {
      a
    }

    fragment null on Foo {
      a
    }

    fragment query on Foo {
      a
    }

    fragment subscription on Foo {
      a
    }

    fragment true on Foo {
      a
    }"

let test_quoted_string () =
  test_query
    "query QuotedString {
      goomba(alias: \"test\\\"\") {
        id
      }
    }"

let suite = [
  "introspection",  `Quick, test_introspection_query;
  "kitchen_sink",   `Quick, test_kitchen_sink;
  "variables",      `Quick, test_variables;
  "default_values", `Quick, test_variables;
  "keywords",       `Quick, test_keywords;
  "quoted_string",  `Quick, test_quoted_string;
]
