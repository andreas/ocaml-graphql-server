[(Graphql_ast.Operation
    { Graphql_ast.optype = Graphql_ast.Query; name = (Some "queryName");
      variable_definitions =
      [{ Graphql_ast.name = "foo";
         typ = (Graphql_ast.NamedType "ComplexType"); default_value = None };
        { Graphql_ast.name = "site"; typ = (Graphql_ast.NamedType "Site");
          default_value = (Some `Enum ("MOBILE")) }
        ];
      directives = [];
      selection_set =
      [(Graphql_ast.Field
          { Graphql_ast.alias = (Some "whoever123is"); name = "node";
            arguments = [("id", `List ([`Int (123); `Int (456)]))];
            directives = [];
            selection_set =
            [(Graphql_ast.Field
                { Graphql_ast.alias = None; name = "id"; arguments = [];
                  directives = []; selection_set = [] });
              (Graphql_ast.InlineFragment
                 { Graphql_ast.type_condition = (Some "User");
                   directives =
                   [{ Graphql_ast.name = "defer"; arguments = [] }];
                   selection_set =
                   [(Graphql_ast.Field
                       { Graphql_ast.alias = None; name = "field2";
                         arguments = []; directives = [];
                         selection_set =
                         [(Graphql_ast.Field
                             { Graphql_ast.alias = None; name = "id";
                               arguments = []; directives = [];
                               selection_set = [] });
                           (Graphql_ast.Field
                              { Graphql_ast.alias = (Some "alias");
                                name = "field1";
                                arguments =
                                [("first", `Int (10));
                                  ("after", `Variable ("foo"))];
                                directives =
                                [{ Graphql_ast.name = "include";
                                   arguments = [("if", `Variable ("foo"))] }
                                  ];
                                selection_set =
                                [(Graphql_ast.Field
                                    { Graphql_ast.alias = None; name = "id";
                                      arguments = []; directives = [];
                                      selection_set = [] });
                                  (Graphql_ast.FragmentSpread
                                     { Graphql_ast.name = "frag";
                                       directives = [] })
                                  ]
                                })
                           ]
                         })
                     ]
                   });
              (Graphql_ast.InlineFragment
                 { Graphql_ast.type_condition = None;
                   directives =
                   [{ Graphql_ast.name = "skip";
                      arguments = [("unless", `Variable ("foo"))] }
                     ];
                   selection_set =
                   [(Graphql_ast.Field
                       { Graphql_ast.alias = None; name = "id";
                         arguments = []; directives = []; selection_set = []
                         })
                     ]
                   });
              (Graphql_ast.InlineFragment
                 { Graphql_ast.type_condition = None; directives = [];
                   selection_set =
                   [(Graphql_ast.Field
                       { Graphql_ast.alias = None; name = "id";
                         arguments = []; directives = []; selection_set = []
                         })
                     ]
                   })
              ]
            })
        ]
      });
  (Graphql_ast.Operation
     { Graphql_ast.optype = Graphql_ast.Mutation; name = (Some "likeStory");
       variable_definitions = []; directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "like";
             arguments = [("story", `Int (123))];
             directives = [{ Graphql_ast.name = "defer"; arguments = [] }];
             selection_set =
             [(Graphql_ast.Field
                 { Graphql_ast.alias = None; name = "story"; arguments = [];
                   directives = [];
                   selection_set =
                   [(Graphql_ast.Field
                       { Graphql_ast.alias = None; name = "id";
                         arguments = []; directives = []; selection_set = []
                         })
                     ]
                   })
               ]
             })
         ]
       });
  (Graphql_ast.Operation
     { Graphql_ast.optype = Graphql_ast.Subscription;
       name = (Some "StoryLikeSubscription");
       variable_definitions =
       [{ Graphql_ast.name = "input";
          typ = (Graphql_ast.NamedType "StoryLikeSubscribeInput");
          default_value = None }
         ];
       directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "storyLikeSubscribe";
             arguments = [("input", `Variable ("input"))]; directives = [];
             selection_set =
             [(Graphql_ast.Field
                 { Graphql_ast.alias = None; name = "story"; arguments = [];
                   directives = [];
                   selection_set =
                   [(Graphql_ast.Field
                       { Graphql_ast.alias = None; name = "likers";
                         arguments = []; directives = [];
                         selection_set =
                         [(Graphql_ast.Field
                             { Graphql_ast.alias = None; name = "count";
                               arguments = []; directives = [];
                               selection_set = [] })
                           ]
                         });
                     (Graphql_ast.Field
                        { Graphql_ast.alias = None; name = "likeSentence";
                          arguments = []; directives = [];
                          selection_set =
                          [(Graphql_ast.Field
                              { Graphql_ast.alias = None; name = "text";
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
  (Graphql_ast.Fragment
     { Graphql_ast.name = "frag"; type_condition = "Friend"; directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "foo";
             arguments =
             [("size", `Variable ("size")); ("bar", `Variable ("b"));
               ("obj", `Assoc ([("key", `String ("value"))]))];
             directives = []; selection_set = [] })
         ]
       });
  (Graphql_ast.Operation
     { Graphql_ast.optype = Graphql_ast.Query; name = None;
       variable_definitions = []; directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "unnamed";
             arguments =
             [("truthy", `Bool (true)); ("falsey", `Bool (false));
               ("nullish", `Null)];
             directives = []; selection_set = [] });
         (Graphql_ast.Field
            { Graphql_ast.alias = None; name = "query"; arguments = [];
              directives = []; selection_set = [] })
         ]
       })
  ]
