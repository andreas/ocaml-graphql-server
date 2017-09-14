[(Graphql_ast.Operation
    { Graphql_ast.optype = Graphql_ast.Query; name = (Some "Keywords");
      variable_definitions =
      [{ Graphql_ast.name = "fragment"; typ = (Graphql_ast.NamedType "Int");
         default_value = None };
        { Graphql_ast.name = "false"; typ = (Graphql_ast.NamedType "Int");
          default_value = None };
        { Graphql_ast.name = "mutation"; typ = (Graphql_ast.NamedType "Int");
          default_value = None };
        { Graphql_ast.name = "null"; typ = (Graphql_ast.NamedType "Int");
          default_value = None };
        { Graphql_ast.name = "on"; typ = (Graphql_ast.NamedType "Int");
          default_value = None };
        { Graphql_ast.name = "query"; typ = (Graphql_ast.NamedType "Int");
          default_value = None };
        { Graphql_ast.name = "subscription";
          typ = (Graphql_ast.NamedType "Int"); default_value = None };
        { Graphql_ast.name = "true"; typ = (Graphql_ast.NamedType "Int");
          default_value = None }
        ];
      directives = [];
      selection_set =
      [(Graphql_ast.Field
          { Graphql_ast.alias = None; name = "fragment"; arguments = [];
            directives = []; selection_set = [] });
        (Graphql_ast.Field
           { Graphql_ast.alias = None; name = "false"; arguments = [];
             directives = []; selection_set = [] });
        (Graphql_ast.Field
           { Graphql_ast.alias = None; name = "mutation"; arguments = [];
             directives = []; selection_set = [] });
        (Graphql_ast.Field
           { Graphql_ast.alias = None; name = "null"; arguments = [];
             directives = []; selection_set = [] });
        (Graphql_ast.Field
           { Graphql_ast.alias = None; name = "on"; arguments = [];
             directives = []; selection_set = [] });
        (Graphql_ast.Field
           { Graphql_ast.alias = None; name = "query"; arguments = [];
             directives = []; selection_set = [] });
        (Graphql_ast.Field
           { Graphql_ast.alias = None; name = "subscription"; arguments = [];
             directives = []; selection_set = [] });
        (Graphql_ast.Field
           { Graphql_ast.alias = None; name = "true"; arguments = [];
             directives = []; selection_set = [] })
        ]
      });
  (Graphql_ast.Fragment
     { Graphql_ast.name = "fragment"; type_condition = "Foo";
       directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "a"; arguments = [];
             directives = []; selection_set = [] })
         ]
       });
  (Graphql_ast.Fragment
     { Graphql_ast.name = "false"; type_condition = "Foo"; directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "a"; arguments = [];
             directives = []; selection_set = [] })
         ]
       });
  (Graphql_ast.Fragment
     { Graphql_ast.name = "mutation"; type_condition = "Foo";
       directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "a"; arguments = [];
             directives = []; selection_set = [] })
         ]
       });
  (Graphql_ast.Fragment
     { Graphql_ast.name = "null"; type_condition = "Foo"; directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "a"; arguments = [];
             directives = []; selection_set = [] })
         ]
       });
  (Graphql_ast.Fragment
     { Graphql_ast.name = "query"; type_condition = "Foo"; directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "a"; arguments = [];
             directives = []; selection_set = [] })
         ]
       });
  (Graphql_ast.Fragment
     { Graphql_ast.name = "subscription"; type_condition = "Foo";
       directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "a"; arguments = [];
             directives = []; selection_set = [] })
         ]
       });
  (Graphql_ast.Fragment
     { Graphql_ast.name = "true"; type_condition = "Foo"; directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "a"; arguments = [];
             directives = []; selection_set = [] })
         ]
       })
  ]
