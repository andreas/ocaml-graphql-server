let graphql_query =
  Alcotest.testable Fmt.string (fun a b ->
      let graphql_ignored = Str.regexp "[ ,\t\r\n]" in
      let strip = Str.global_replace graphql_ignored "" in
      strip a = strip b)

let test_query ?expect query =
  match Graphql_parser.parse query with
  | Ok doc ->
      let query' = Fmt.to_to_string Graphql_parser.pp_document doc in
      Alcotest.check graphql_query "Parse result"
        (Option.value ~default:query expect)
        query'
  | Error err -> Alcotest.failf "Failed to parse %s: %s" query err

let test_introspection_query () =
  test_query
    {|
    query IntrospectionQuery {
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
    }
  |}

let test_kitchen_sink () =
  test_query
    {|
    query queryName($foo: ComplexType, $site: Site = MOBILE) {
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
      foo(size: $size, bar: $b, obj: {key: "value"})
    }

    {
      unnamed(truthy: true, falsey: false, nullish: null),
      query
    }
  |}

let test_variables () =
  test_query
    {|
    query Named($a: String, $b: Float) {
      x
    }

    query List($a: [Bool], $b: [[Int]]) {
      x
    }

    query NonNull($a: ID!, $b: [Foo]!, $c: [Foo!], $d: [Foo!]!) {
      x
    }
  |}

let test_default_values () =
  test_query
    {|
    query DefaultValues(
        $a: Int = 1,
        $b: [String] = ["a"],
        $c: Obj = {x: {y: true}, z: RED},
        $d: [Obj]! = [{x: {y: null}, z: BLUE}]
      ) {
      x
    }
  |}

let test_keywords () =
  test_query
    {|
    query Keywords(
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
    }
  |}

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

let test_block_string () =
  let join_lines lines = String.concat "\n" lines in
  let make_field field_name lines =
    Format.sprintf {|%s(arg: """%s""")|} field_name (join_lines lines)
  in

  let query =
    Format.sprintf "{\n  %s\n}"
      (String.concat "\n  "
         [
           make_field "uniform_indentation"
             [
               "";
               "    Hello,";
               "      World!";
               "";
               "    Yours,";
               "      GraphQL.";
             ];
           make_field "removes_empty_leading_and_trailing_lines"
             [
               "";
               "";
               "    Hello,";
               "      World!";
               "";
               "    Yours,";
               "      GraphQL.";
               "";
               "";
             ];
           make_field "removes_blank_leading_and_trailing_lines"
             [
               "  ";
               "        ";
               "    Hello,";
               "      World!";
               "";
               "    Yours,";
               "      GraphQL.";
               "        ";
               "  ";
             ];
           make_field "retains_indentation_of_first_line"
             [
               "    Hello,"; "      World!"; ""; "    Yours,"; "      GraphQL.";
             ];
           make_field "does_not_alter_trailing_spaces"
             [
               "               ";
               "    Hello,     ";
               "      World!   ";
               "               ";
               "    Yours,     ";
               "      GraphQL. ";
               "               ";
             ];
           make_field "empty_string" [ "" ];
           make_field "do_not_escape_characters" [ {|" \\ / \b \f \n \r \t|} ];
         ])
  in
  test_query
    ~expect:
      {|
        {
          uniform_indentation(arg: "Hello,\n  World!\n\nYours,\n  GraphQL.")
          removes_empty_leading_and_trailing_lines(arg: "Hello,\n  World!\n\nYours,\n  GraphQL.")
          removes_blank_leading_and_trailing_lines(arg: "Hello,\n  World!\n\nYours,\n  GraphQL.")
          retains_indentation_of_first_line(arg: "    Hello,\n  World!\n\nYours,\n  GraphQL.")
          does_not_alter_trailing_spaces(arg: "Hello,     \n  World!   \n           \nYours,     \n  GraphQL. ")
          empty_string(arg: "")
          do_not_escape_characters(arg: "\" \\\\ / \\b \\f \\n \\r \\t")
        }
      |}
    query

let suite =
  [
    ("introspection", `Quick, test_introspection_query);
    ("kitchen sink", `Quick, test_kitchen_sink);
    ("default values", `Quick, test_default_values);
    ("keywords", `Quick, test_keywords);
    ("variables", `Quick, test_variables);
    ("escaped string", `Quick, test_escaped_string);
    ("block string", `Quick, test_block_string);
  ]
