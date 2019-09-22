let graphql_query =
  Alcotest.testable Fmt.string (fun a b ->
      let graphql_ignored = Str.regexp "[ ,\t\r\n]" in
      let strip = Str.global_replace graphql_ignored "" in
      strip a = strip b)

let test_query query =
  match Graphql_parser.parse query with
  | Ok doc ->
      let query' = Fmt.to_to_string Graphql_parser.pp_document doc in
      Alcotest.check graphql_query "Parse result" query query'
  | Error err -> Alcotest.failf "Failed to parse %s: %s" query err

let test_introspection_query () =
  test_query
    "query IntrospectionQuery {\n\
    \      __schema {\n\
    \        queryType { name }\n\
    \        mutationType { name }\n\
    \        subscriptionType { name }\n\
    \        types {\n\
    \          ...FullType\n\
    \        }\n\
    \        directives {\n\
    \          name\n\
    \          description\n\
    \          args {\n\
    \            ...InputValue\n\
    \          }\n\
    \          onOperation\n\
    \          onFragment\n\
    \          onField\n\
    \        }\n\
    \      }\n\
    \    }\n\n\
    \    fragment FullType on __Type {\n\
    \      kind\n\
    \      name\n\
    \      description\n\
    \      fields(includeDeprecated: true) {\n\
    \        name\n\
    \        description\n\
    \        args {\n\
    \          ...InputValue\n\
    \        }\n\
    \        type {\n\
    \          ...TypeRef\n\
    \        }\n\
    \        isDeprecated\n\
    \        deprecationReason\n\
    \      }\n\
    \      inputFields {\n\
    \        ...InputValue\n\
    \      }\n\
    \      interfaces {\n\
    \        ...TypeRef\n\
    \      }\n\
    \      enumValues(includeDeprecated: true) {\n\
    \        name\n\
    \        description\n\
    \        isDeprecated\n\
    \        deprecationReason\n\
    \      }\n\
    \      possibleTypes {\n\
    \        ...TypeRef\n\
    \      }\n\
    \    }\n\n\
    \    fragment InputValue on __InputValue {\n\
    \      name\n\
    \      description\n\
    \      type { ...TypeRef }\n\
    \      defaultValue\n\
    \    }\n\n\
    \    fragment TypeRef on __Type {\n\
    \      kind\n\
    \      name\n\
    \      ofType {\n\
    \        kind\n\
    \        name\n\
    \        ofType {\n\
    \          kind\n\
    \          name\n\
    \          ofType {\n\
    \            kind\n\
    \            name\n\
    \          }\n\
    \        }\n\
    \      }\n\
    \    }"

let test_kitchen_sink () =
  test_query
    "query queryName($foo: ComplexType, $site: Site = MOBILE) {\n\
    \      whoever123is: node(id: [123, 456]) {\n\
    \        id ,\n\
    \        ... on User @defer {\n\
    \          field2 {\n\
    \            id ,\n\
    \            alias: field1(first:10, after:$foo,) @include(if: $foo) {\n\
    \              id,\n\
    \              ...frag\n\
    \            }\n\
    \          }\n\
    \        }\n\
    \        ... @skip(unless: $foo) {\n\
    \          id\n\
    \        }\n\
    \        ... {\n\
    \          id\n\
    \        }\n\
    \      }\n\
    \    }\n\n\
    \    mutation likeStory {\n\
    \      like(story: 123) @defer {\n\
    \        story {\n\
    \          id\n\
    \        }\n\
    \      }\n\
    \    }\n\n\
    \    subscription StoryLikeSubscription($input: StoryLikeSubscribeInput) {\n\
    \      storyLikeSubscribe(input: $input) {\n\
    \        story {\n\
    \          likers {\n\
    \            count\n\
    \          }\n\
    \          likeSentence {\n\
    \            text\n\
    \          }\n\
    \        }\n\
    \      }\n\
    \    }\n\n\
    \    fragment frag on Friend {\n\
    \      foo(size: $size, bar: $b, obj: {key: \"value\"})\n\
    \    }\n\n\
    \    {\n\
    \      unnamed(truthy: true, falsey: false, nullish: null),\n\
    \      query\n\
    \    }"

let test_variables () =
  test_query
    "query Named($a: String, $b: Float) {\n\
    \      x\n\
    \    }\n\n\
    \    query List($a: [Bool], $b: [[Int]]) {\n\
    \      x\n\
    \    }\n\n\
    \    query NonNull($a: ID!, $b: [Foo]!, $c: [Foo!], $d: [Foo!]!) {\n\
    \      x\n\
    \    }"

let test_default_values () =
  test_query
    "query DefaultValues(\n\
    \        $a: Int = 1,\n\
    \        $b: [String] = [\"a\"],\n\
    \        $c: Obj = {x: {y: true}, z: RED},\n\
    \        $d: [Obj]! = [{x: {y: null}, z: BLUE}]\n\
    \      ) {\n\
    \      x\n\
    \    }"

let test_keywords () =
  test_query
    "query Keywords(\n\
    \      $fragment: Int,\n\
    \      $false: Int,\n\
    \      $mutation: Int,\n\
    \      $null: Int,\n\
    \      $on: Int,\n\
    \      $query: Int,\n\
    \      $subscription: Int,\n\
    \      $true: Int\n\
    \    ) {\n\
    \      fragment\n\
    \      false\n\
    \      mutation\n\
    \      null\n\
    \      on\n\
    \      query\n\
    \      subscription\n\
    \      true\n\
    \    }\n\n\
    \    fragment fragment on Foo {\n\
    \      a\n\
    \    }\n\n\
    \    fragment false on Foo {\n\
    \      a\n\
    \    }\n\n\
    \    fragment mutation on Foo {\n\
    \      a\n\
    \    }\n\n\
    \    fragment null on Foo {\n\
    \      a\n\
    \    }\n\n\
    \    fragment query on Foo {\n\
    \      a\n\
    \    }\n\n\
    \    fragment subscription on Foo {\n\
    \      a\n\
    \    }\n\n\
    \    fragment true on Foo {\n\
    \      a\n\
    \    }"

let test_escaped_string () =
  test_query
    {|
    {	
      escaped_quote(x: "\"")
      backslash(x: "\\")
      slash(x: "/")
      escaped_backspace(x: "\b")
      escaped_form_feed(x: "\f")
      escaped_newline(x: "\n")
      escaped_carriage_return(x: "\r")
      tab(x: "  ")
      escaped_tab(x: "\t")
    }
  |}

let suite =
  [
    ("introspection", `Quick, test_introspection_query);
    ("kitchen sink", `Quick, test_kitchen_sink);
    ("default values", `Quick, test_default_values);
    ("keywords", `Quick, test_keywords);
    ("variables", `Quick, test_variables);
    ("escaped string", `Quick, test_escaped_string);
  ]
