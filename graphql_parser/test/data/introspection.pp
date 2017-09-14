[(Graphql_ast.Operation
    { Graphql_ast.optype = Graphql_ast.Query;
      name = (Some "IntrospectionQuery"); variable_definitions = [];
      directives = [];
      selection_set =
      [(Graphql_ast.Field
          { Graphql_ast.alias = None; name = "__schema"; arguments = [];
            directives = [];
            selection_set =
            [(Graphql_ast.Field
                { Graphql_ast.alias = None; name = "queryType";
                  arguments = []; directives = [];
                  selection_set =
                  [(Graphql_ast.Field
                      { Graphql_ast.alias = None; name = "name";
                        arguments = []; directives = []; selection_set = [] })
                    ]
                  });
              (Graphql_ast.Field
                 { Graphql_ast.alias = None; name = "mutationType";
                   arguments = []; directives = [];
                   selection_set =
                   [(Graphql_ast.Field
                       { Graphql_ast.alias = None; name = "name";
                         arguments = []; directives = []; selection_set = []
                         })
                     ]
                   });
              (Graphql_ast.Field
                 { Graphql_ast.alias = None; name = "subscriptionType";
                   arguments = []; directives = [];
                   selection_set =
                   [(Graphql_ast.Field
                       { Graphql_ast.alias = None; name = "name";
                         arguments = []; directives = []; selection_set = []
                         })
                     ]
                   });
              (Graphql_ast.Field
                 { Graphql_ast.alias = None; name = "types"; arguments = [];
                   directives = [];
                   selection_set =
                   [(Graphql_ast.FragmentSpread
                       { Graphql_ast.name = "FullType"; directives = [] })
                     ]
                   });
              (Graphql_ast.Field
                 { Graphql_ast.alias = None; name = "directives";
                   arguments = []; directives = [];
                   selection_set =
                   [(Graphql_ast.Field
                       { Graphql_ast.alias = None; name = "name";
                         arguments = []; directives = []; selection_set = []
                         });
                     (Graphql_ast.Field
                        { Graphql_ast.alias = None; name = "description";
                          arguments = []; directives = []; selection_set = []
                          });
                     (Graphql_ast.Field
                        { Graphql_ast.alias = None; name = "args";
                          arguments = []; directives = [];
                          selection_set =
                          [(Graphql_ast.FragmentSpread
                              { Graphql_ast.name = "InputValue";
                                directives = [] })
                            ]
                          });
                     (Graphql_ast.Field
                        { Graphql_ast.alias = None; name = "onOperation";
                          arguments = []; directives = []; selection_set = []
                          });
                     (Graphql_ast.Field
                        { Graphql_ast.alias = None; name = "onFragment";
                          arguments = []; directives = []; selection_set = []
                          });
                     (Graphql_ast.Field
                        { Graphql_ast.alias = None; name = "onField";
                          arguments = []; directives = []; selection_set = []
                          })
                     ]
                   })
              ]
            })
        ]
      });
  (Graphql_ast.Fragment
     { Graphql_ast.name = "FullType"; type_condition = "__Type";
       directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "kind"; arguments = [];
             directives = []; selection_set = [] });
         (Graphql_ast.Field
            { Graphql_ast.alias = None; name = "name"; arguments = [];
              directives = []; selection_set = [] });
         (Graphql_ast.Field
            { Graphql_ast.alias = None; name = "description"; arguments = [];
              directives = []; selection_set = [] });
         (Graphql_ast.Field
            { Graphql_ast.alias = None; name = "fields";
              arguments = [("includeDeprecated", `Bool (true))];
              directives = [];
              selection_set =
              [(Graphql_ast.Field
                  { Graphql_ast.alias = None; name = "name"; arguments = [];
                    directives = []; selection_set = [] });
                (Graphql_ast.Field
                   { Graphql_ast.alias = None; name = "description";
                     arguments = []; directives = []; selection_set = [] });
                (Graphql_ast.Field
                   { Graphql_ast.alias = None; name = "args"; arguments = [];
                     directives = [];
                     selection_set =
                     [(Graphql_ast.FragmentSpread
                         { Graphql_ast.name = "InputValue"; directives = [] })
                       ]
                     });
                (Graphql_ast.Field
                   { Graphql_ast.alias = None; name = "type"; arguments = [];
                     directives = [];
                     selection_set =
                     [(Graphql_ast.FragmentSpread
                         { Graphql_ast.name = "TypeRef"; directives = [] })
                       ]
                     });
                (Graphql_ast.Field
                   { Graphql_ast.alias = None; name = "isDeprecated";
                     arguments = []; directives = []; selection_set = [] });
                (Graphql_ast.Field
                   { Graphql_ast.alias = None; name = "deprecationReason";
                     arguments = []; directives = []; selection_set = [] })
                ]
              });
         (Graphql_ast.Field
            { Graphql_ast.alias = None; name = "inputFields"; arguments = [];
              directives = [];
              selection_set =
              [(Graphql_ast.FragmentSpread
                  { Graphql_ast.name = "InputValue"; directives = [] })
                ]
              });
         (Graphql_ast.Field
            { Graphql_ast.alias = None; name = "interfaces"; arguments = [];
              directives = [];
              selection_set =
              [(Graphql_ast.FragmentSpread
                  { Graphql_ast.name = "TypeRef"; directives = [] })
                ]
              });
         (Graphql_ast.Field
            { Graphql_ast.alias = None; name = "enumValues";
              arguments = [("includeDeprecated", `Bool (true))];
              directives = [];
              selection_set =
              [(Graphql_ast.Field
                  { Graphql_ast.alias = None; name = "name"; arguments = [];
                    directives = []; selection_set = [] });
                (Graphql_ast.Field
                   { Graphql_ast.alias = None; name = "description";
                     arguments = []; directives = []; selection_set = [] });
                (Graphql_ast.Field
                   { Graphql_ast.alias = None; name = "isDeprecated";
                     arguments = []; directives = []; selection_set = [] });
                (Graphql_ast.Field
                   { Graphql_ast.alias = None; name = "deprecationReason";
                     arguments = []; directives = []; selection_set = [] })
                ]
              });
         (Graphql_ast.Field
            { Graphql_ast.alias = None; name = "possibleTypes";
              arguments = []; directives = [];
              selection_set =
              [(Graphql_ast.FragmentSpread
                  { Graphql_ast.name = "TypeRef"; directives = [] })
                ]
              })
         ]
       });
  (Graphql_ast.Fragment
     { Graphql_ast.name = "InputValue"; type_condition = "__InputValue";
       directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "name"; arguments = [];
             directives = []; selection_set = [] });
         (Graphql_ast.Field
            { Graphql_ast.alias = None; name = "description"; arguments = [];
              directives = []; selection_set = [] });
         (Graphql_ast.Field
            { Graphql_ast.alias = None; name = "type"; arguments = [];
              directives = [];
              selection_set =
              [(Graphql_ast.FragmentSpread
                  { Graphql_ast.name = "TypeRef"; directives = [] })
                ]
              });
         (Graphql_ast.Field
            { Graphql_ast.alias = None; name = "defaultValue";
              arguments = []; directives = []; selection_set = [] })
         ]
       });
  (Graphql_ast.Fragment
     { Graphql_ast.name = "TypeRef"; type_condition = "__Type";
       directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "kind"; arguments = [];
             directives = []; selection_set = [] });
         (Graphql_ast.Field
            { Graphql_ast.alias = None; name = "name"; arguments = [];
              directives = []; selection_set = [] });
         (Graphql_ast.Field
            { Graphql_ast.alias = None; name = "ofType"; arguments = [];
              directives = [];
              selection_set =
              [(Graphql_ast.Field
                  { Graphql_ast.alias = None; name = "kind"; arguments = [];
                    directives = []; selection_set = [] });
                (Graphql_ast.Field
                   { Graphql_ast.alias = None; name = "name"; arguments = [];
                     directives = []; selection_set = [] });
                (Graphql_ast.Field
                   { Graphql_ast.alias = None; name = "ofType";
                     arguments = []; directives = [];
                     selection_set =
                     [(Graphql_ast.Field
                         { Graphql_ast.alias = None; name = "kind";
                           arguments = []; directives = [];
                           selection_set = [] });
                       (Graphql_ast.Field
                          { Graphql_ast.alias = None; name = "name";
                            arguments = []; directives = [];
                            selection_set = [] });
                       (Graphql_ast.Field
                          { Graphql_ast.alias = None; name = "ofType";
                            arguments = []; directives = [];
                            selection_set =
                            [(Graphql_ast.Field
                                { Graphql_ast.alias = None; name = "kind";
                                  arguments = []; directives = [];
                                  selection_set = [] });
                              (Graphql_ast.Field
                                 { Graphql_ast.alias = None; name = "name";
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
