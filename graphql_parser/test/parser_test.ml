let test_query query expected =
  match Graphql_parser.parse query with
  | Ok doc ->
    if doc = expected then
      ()
    else
      Alcotest.failf "Invalid parse result!"
  | Error err ->
    Alcotest.failf "Failed to parse %s: %s" query err

let test_introspection_query () =
  let query =
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
  and expected =
    [(Graphql_parser.Operation
        { Graphql_parser.optype = Graphql_parser.Query;
          name = (Some "IntrospectionQuery"); variable_definitions = [];
          directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "__schema"; arguments = [];
                  directives = [];
                  selection_set =
                    [(Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "queryType";
                          arguments = []; directives = [];
                          selection_set =
                            [(Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "name";
                                  arguments = []; directives = []; selection_set = [] })
                            ]
                        });
                     (Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "mutationType";
                          arguments = []; directives = [];
                          selection_set =
                            [(Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "name";
                                  arguments = []; directives = []; selection_set = []
                                })
                            ]
                        });
                     (Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "subscriptionType";
                          arguments = []; directives = [];
                          selection_set =
                            [(Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "name";
                                  arguments = []; directives = []; selection_set = []
                                })
                            ]
                        });
                     (Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "types"; arguments = [];
                          directives = [];
                          selection_set =
                            [(Graphql_parser.FragmentSpread
                                { Graphql_parser.name = "FullType"; directives = [] })
                            ]
                        });
                     (Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "directives";
                          arguments = []; directives = [];
                          selection_set =
                            [(Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "name";
                                  arguments = []; directives = []; selection_set = []
                                });
                             (Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "description";
                                  arguments = []; directives = []; selection_set = []
                                });
                             (Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "args";
                                  arguments = []; directives = [];
                                  selection_set =
                                    [(Graphql_parser.FragmentSpread
                                        { Graphql_parser.name = "InputValue";
                                          directives = [] })
                                    ]
                                });
                             (Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "onOperation";
                                  arguments = []; directives = []; selection_set = []
                                });
                             (Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "onFragment";
                                  arguments = []; directives = []; selection_set = []
                                });
                             (Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "onField";
                                  arguments = []; directives = []; selection_set = []
                                })
                            ]
                        })
                    ]
                })
            ]
        });
     (Graphql_parser.Fragment
        { Graphql_parser.name = "FullType"; type_condition = "__Type";
          directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "kind"; arguments = [];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "name"; arguments = [];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "description"; arguments = [];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "fields";
                  arguments = [("includeDeprecated", `Bool (true))];
                  directives = [];
                  selection_set =
                    [(Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "name"; arguments = [];
                          directives = []; selection_set = [] });
                     (Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "description";
                          arguments = []; directives = []; selection_set = [] });
                     (Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "args"; arguments = [];
                          directives = [];
                          selection_set =
                            [(Graphql_parser.FragmentSpread
                                { Graphql_parser.name = "InputValue"; directives = [] })
                            ]
                        });
                     (Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "type"; arguments = [];
                          directives = [];
                          selection_set =
                            [(Graphql_parser.FragmentSpread
                                { Graphql_parser.name = "TypeRef"; directives = [] })
                            ]
                        });
                     (Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "isDeprecated";
                          arguments = []; directives = []; selection_set = [] });
                     (Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "deprecationReason";
                          arguments = []; directives = []; selection_set = [] })
                    ]
                });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "inputFields"; arguments = [];
                  directives = [];
                  selection_set =
                    [(Graphql_parser.FragmentSpread
                        { Graphql_parser.name = "InputValue"; directives = [] })
                    ]
                });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "interfaces"; arguments = [];
                  directives = [];
                  selection_set =
                    [(Graphql_parser.FragmentSpread
                        { Graphql_parser.name = "TypeRef"; directives = [] })
                    ]
                });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "enumValues";
                  arguments = [("includeDeprecated", `Bool (true))];
                  directives = [];
                  selection_set =
                    [(Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "name"; arguments = [];
                          directives = []; selection_set = [] });
                     (Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "description";
                          arguments = []; directives = []; selection_set = [] });
                     (Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "isDeprecated";
                          arguments = []; directives = []; selection_set = [] });
                     (Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "deprecationReason";
                          arguments = []; directives = []; selection_set = [] })
                    ]
                });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "possibleTypes";
                  arguments = []; directives = [];
                  selection_set =
                    [(Graphql_parser.FragmentSpread
                        { Graphql_parser.name = "TypeRef"; directives = [] })
                    ]
                })
            ]
        });
     (Graphql_parser.Fragment
        { Graphql_parser.name = "InputValue"; type_condition = "__InputValue";
          directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "name"; arguments = [];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "description"; arguments = [];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "type"; arguments = [];
                  directives = [];
                  selection_set =
                    [(Graphql_parser.FragmentSpread
                        { Graphql_parser.name = "TypeRef"; directives = [] })
                    ]
                });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "defaultValue";
                  arguments = []; directives = []; selection_set = [] })
            ]
        });
     (Graphql_parser.Fragment
        { Graphql_parser.name = "TypeRef"; type_condition = "__Type";
          directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "kind"; arguments = [];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "name"; arguments = [];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "ofType"; arguments = [];
                  directives = [];
                  selection_set =
                    [(Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "kind"; arguments = [];
                          directives = []; selection_set = [] });
                     (Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "name"; arguments = [];
                          directives = []; selection_set = [] });
                     (Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "ofType";
                          arguments = []; directives = [];
                          selection_set =
                            [(Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "kind";
                                  arguments = []; directives = [];
                                  selection_set = [] });
                             (Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "name";
                                  arguments = []; directives = [];
                                  selection_set = [] });
                             (Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "ofType";
                                  arguments = []; directives = [];
                                  selection_set =
                                    [(Graphql_parser.Field
                                        { Graphql_parser.alias = None; name = "kind";
                                          arguments = []; directives = [];
                                          selection_set = [] });
                                     (Graphql_parser.Field
                                        { Graphql_parser.alias = None; name = "name";
                                          arguments = []; directives = [];
                                          selection_set = [] })
                                    ]
                                })
                            ]
                        })
                    ]
                })
            ]
        })
    ]
  in test_query query expected

let test_kitchen_sink () =
  let query =
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
  and expected =
    [(Graphql_parser.Operation
        { Graphql_parser.optype = Graphql_parser.Query; name = (Some "queryName");
          variable_definitions =
            [{ Graphql_parser.name = "foo";
               typ = (Graphql_parser.NamedType "ComplexType"); default_value = None };
             { Graphql_parser.name = "site"; typ = (Graphql_parser.NamedType "Site");
               default_value = (Some (`Enum "MOBILE")) }
            ];
          directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = (Some "whoever123is"); name = "node";
                  arguments = [("id", `List ([`Int (123); `Int (456)]))];
                  directives = [];
                  selection_set =
                    [(Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "id"; arguments = [];
                          directives = []; selection_set = [] });
                     (Graphql_parser.InlineFragment
                        { Graphql_parser.type_condition = (Some "User");
                          directives =
                            [{ Graphql_parser.name = "defer"; arguments = [] }];
                          selection_set =
                            [(Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "field2";
                                  arguments = []; directives = [];
                                  selection_set =
                                    [(Graphql_parser.Field
                                        { Graphql_parser.alias = None; name = "id";
                                          arguments = []; directives = [];
                                          selection_set = [] });
                                     (Graphql_parser.Field
                                        { Graphql_parser.alias = (Some "alias");
                                          name = "field1";
                                          arguments =
                                            [("first", `Int (10));
                                             ("after", `Variable ("foo"))];
                                          directives =
                                            [{ Graphql_parser.name = "include";
                                               arguments = [("if", `Variable ("foo"))] }
                                            ];
                                          selection_set =
                                            [(Graphql_parser.Field
                                                { Graphql_parser.alias = None; name = "id";
                                                  arguments = []; directives = [];
                                                  selection_set = [] });
                                             (Graphql_parser.FragmentSpread
                                                { Graphql_parser.name = "frag";
                                                  directives = [] })
                                            ]
                                        })
                                    ]
                                })
                            ]
                        });
                     (Graphql_parser.InlineFragment
                        { Graphql_parser.type_condition = None;
                          directives =
                            [{ Graphql_parser.name = "skip";
                               arguments = [("unless", `Variable ("foo"))] }
                            ];
                          selection_set =
                            [(Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "id";
                                  arguments = []; directives = []; selection_set = []
                                })
                            ]
                        });
                     (Graphql_parser.InlineFragment
                        { Graphql_parser.type_condition = None; directives = [];
                          selection_set =
                            [(Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "id";
                                  arguments = []; directives = []; selection_set = []
                                })
                            ]
                        })
                    ]
                })
            ]
        });
     (Graphql_parser.Operation
        { Graphql_parser.optype = Graphql_parser.Mutation; name = (Some "likeStory");
          variable_definitions = []; directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "like";
                  arguments = [("story", `Int (123))];
                  directives = [{ Graphql_parser.name = "defer"; arguments = [] }];
                  selection_set =
                    [(Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "story"; arguments = [];
                          directives = [];
                          selection_set =
                            [(Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "id";
                                  arguments = []; directives = []; selection_set = []
                                })
                            ]
                        })
                    ]
                })
            ]
        });
     (Graphql_parser.Operation
        { Graphql_parser.optype = Graphql_parser.Subscription;
          name = (Some "StoryLikeSubscription");
          variable_definitions =
            [{ Graphql_parser.name = "input";
               typ = (Graphql_parser.NamedType "StoryLikeSubscribeInput");
               default_value = None }
            ];
          directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "storyLikeSubscribe";
                  arguments = [("input", `Variable ("input"))]; directives = [];
                  selection_set =
                    [(Graphql_parser.Field
                        { Graphql_parser.alias = None; name = "story"; arguments = [];
                          directives = [];
                          selection_set =
                            [(Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "likers";
                                  arguments = []; directives = [];
                                  selection_set =
                                    [(Graphql_parser.Field
                                        { Graphql_parser.alias = None; name = "count";
                                          arguments = []; directives = [];
                                          selection_set = [] })
                                    ]
                                });
                             (Graphql_parser.Field
                                { Graphql_parser.alias = None; name = "likeSentence";
                                  arguments = []; directives = [];
                                  selection_set =
                                    [(Graphql_parser.Field
                                        { Graphql_parser.alias = None; name = "text";
                                          arguments = []; directives = [];
                                          selection_set = [] })
                                    ]
                                })
                            ]
                        })
                    ]
                })
            ]
        });
     (Graphql_parser.Fragment
        { Graphql_parser.name = "frag"; type_condition = "Friend"; directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "foo";
                  arguments =
                    [("size", `Variable ("size")); ("bar", `Variable ("b"));
                     ("obj", `Assoc ([("key", `String ("value"))]))];
                  directives = []; selection_set = [] })
            ]
        });
     (Graphql_parser.Operation
        { Graphql_parser.optype = Graphql_parser.Query; name = None;
          variable_definitions = []; directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "unnamed";
                  arguments =
                    [("truthy", `Bool (true)); ("falsey", `Bool (false));
                     ("nullish", `Null)];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "query"; arguments = [];
                  directives = []; selection_set = [] })
            ]
        })
    ]
  in test_query query expected

let test_variables () =
  let query = 
    "query Named($a: String, $b: Float) {
      x
    }

    query List($a: [Bool], $b: [[Int]]) {
      x
    }

    query NonNull($a: ID!, $b: [Foo]!, $c: [Foo!], $d: [Foo!]!) {
      x
    }"
  and expected =
    [(Graphql_parser.Operation
        { Graphql_parser.optype = Graphql_parser.Query; name = (Some "Named");
          variable_definitions =
            [{ Graphql_parser.name = "a"; typ = (Graphql_parser.NamedType "String");
               default_value = None };
             { Graphql_parser.name = "b"; typ = (Graphql_parser.NamedType "Float");
               default_value = None }
            ];
          directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "x"; arguments = [];
                  directives = []; selection_set = [] })
            ]
        });
     (Graphql_parser.Operation
        { Graphql_parser.optype = Graphql_parser.Query; name = (Some "List");
          variable_definitions =
            [{ Graphql_parser.name = "a";
               typ = (Graphql_parser.ListType (Graphql_parser.NamedType "Bool"));
               default_value = None };
             { Graphql_parser.name = "b";
               typ =
                 (Graphql_parser.ListType
                    (Graphql_parser.ListType (Graphql_parser.NamedType "Int")));
               default_value = None }
            ];
          directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "x"; arguments = [];
                  directives = []; selection_set = [] })
            ]
        });
     (Graphql_parser.Operation
        { Graphql_parser.optype = Graphql_parser.Query; name = (Some "NonNull");
          variable_definitions =
            [{ Graphql_parser.name = "a";
               typ = (Graphql_parser.NonNullType (Graphql_parser.NamedType "ID"));
               default_value = None };
             { Graphql_parser.name = "b";
               typ =
                 (Graphql_parser.NonNullType
                    (Graphql_parser.ListType (Graphql_parser.NamedType "Foo")));
               default_value = None };
             { Graphql_parser.name = "c";
               typ =
                 (Graphql_parser.ListType
                    (Graphql_parser.NonNullType (Graphql_parser.NamedType "Foo")));
               default_value = None };
             { Graphql_parser.name = "d";
               typ =
                 (Graphql_parser.NonNullType
                    (Graphql_parser.ListType
                       (Graphql_parser.NonNullType (Graphql_parser.NamedType "Foo"))));
               default_value = None }
            ];
          directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "x"; arguments = [];
                  directives = []; selection_set = [] })
            ]
        })
    ]
  in test_query query expected

let test_default_values () =
  let query =
    "query DefaultValues(
        $a: Int = 1,
        $b: [String] = [\"a\"],
        $c: Obj = {x: {y: true}, z: RED},
        $d: [Obj]! = [{x: {y: null}, z: BLUE}]
      ) {
      x
    }"
  and expected =
    [(Graphql_parser.Operation
        { Graphql_parser.optype = Graphql_parser.Query; name = (Some "DefaultValues");
          variable_definitions =
            [{ Graphql_parser.name = "a"; typ = (Graphql_parser.NamedType "Int");
               default_value = Some (`Int 1) };
             { Graphql_parser.name = "b";
               typ = (Graphql_parser.ListType (Graphql_parser.NamedType "String"));
               default_value = (Some (`List [`String ("a")])) };
             { Graphql_parser.name = "c"; typ = (Graphql_parser.NamedType "Obj");
               default_value =
                 (Some (`Assoc [("x", `Assoc ([("y", `Bool (true))]));
                                ("z", `Enum ("RED"))]))
             };
             { Graphql_parser.name = "d";
               typ =
                 (Graphql_parser.NonNullType
                    (Graphql_parser.ListType (Graphql_parser.NamedType "Obj")));
               default_value =
                 (Some (`List [`Assoc ([("x", `Assoc ([("y", `Null)]));
                                        ("z", `Enum ("BLUE"))])
                              ]))
             }
            ];
          directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "x"; arguments = [];
                  directives = []; selection_set = [] })
            ]
        })
    ]
  in test_query query expected

let test_keywords () =
  let query =
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
  and expected =
    [(Graphql_parser.Operation
        { Graphql_parser.optype = Graphql_parser.Query; name = (Some "Keywords");
          variable_definitions =
            [{ Graphql_parser.name = "fragment"; typ = (Graphql_parser.NamedType "Int");
               default_value = None };
             { Graphql_parser.name = "false"; typ = (Graphql_parser.NamedType "Int");
               default_value = None };
             { Graphql_parser.name = "mutation"; typ = (Graphql_parser.NamedType "Int");
               default_value = None };
             { Graphql_parser.name = "null"; typ = (Graphql_parser.NamedType "Int");
               default_value = None };
             { Graphql_parser.name = "on"; typ = (Graphql_parser.NamedType "Int");
               default_value = None };
             { Graphql_parser.name = "query"; typ = (Graphql_parser.NamedType "Int");
               default_value = None };
             { Graphql_parser.name = "subscription";
               typ = (Graphql_parser.NamedType "Int"); default_value = None };
             { Graphql_parser.name = "true"; typ = (Graphql_parser.NamedType "Int");
               default_value = None }
            ];
          directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "fragment"; arguments = [];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "false"; arguments = [];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "mutation"; arguments = [];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "null"; arguments = [];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "on"; arguments = [];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "query"; arguments = [];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "subscription"; arguments = [];
                  directives = []; selection_set = [] });
             (Graphql_parser.Field
                { Graphql_parser.alias = None; name = "true"; arguments = [];
                  directives = []; selection_set = [] })
            ]
        });
     (Graphql_parser.Fragment
        { Graphql_parser.name = "fragment"; type_condition = "Foo";
          directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "a"; arguments = [];
                  directives = []; selection_set = [] })
            ]
        });
     (Graphql_parser.Fragment
        { Graphql_parser.name = "false"; type_condition = "Foo"; directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "a"; arguments = [];
                  directives = []; selection_set = [] })
            ]
        });
     (Graphql_parser.Fragment
        { Graphql_parser.name = "mutation"; type_condition = "Foo";
          directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "a"; arguments = [];
                  directives = []; selection_set = [] })
            ]
        });
     (Graphql_parser.Fragment
        { Graphql_parser.name = "null"; type_condition = "Foo"; directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "a"; arguments = [];
                  directives = []; selection_set = [] })
            ]
        });
     (Graphql_parser.Fragment
        { Graphql_parser.name = "query"; type_condition = "Foo"; directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "a"; arguments = [];
                  directives = []; selection_set = [] })
            ]
        });
     (Graphql_parser.Fragment
        { Graphql_parser.name = "subscription"; type_condition = "Foo";
          directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "a"; arguments = [];
                  directives = []; selection_set = [] })
            ]
        });
     (Graphql_parser.Fragment
        { Graphql_parser.name = "true"; type_condition = "Foo"; directives = [];
          selection_set =
            [(Graphql_parser.Field
                { Graphql_parser.alias = None; name = "a"; arguments = [];
                  directives = []; selection_set = [] })
            ]
        })
    ]
  in test_query query expected

let suite = [
  "introspection", `Quick, test_introspection_query;
  "kitchen sink", `Quick, test_kitchen_sink;
  "default values", `Quick, test_default_values;
  "keywords", `Quick, test_keywords;
  "variables", `Quick, test_variables;
]
