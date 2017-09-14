[(Graphql_ast.Operation
    { Graphql_ast.optype = Graphql_ast.Query; name = (Some "Named");
      variable_definitions =
      [{ Graphql_ast.name = "a"; typ = (Graphql_ast.NamedType "String");
         default_value = None };
        { Graphql_ast.name = "b"; typ = (Graphql_ast.NamedType "Float");
          default_value = None }
        ];
      directives = [];
      selection_set =
      [(Graphql_ast.Field
          { Graphql_ast.alias = None; name = "x"; arguments = [];
            directives = []; selection_set = [] })
        ]
      });
  (Graphql_ast.Operation
     { Graphql_ast.optype = Graphql_ast.Query; name = (Some "List");
       variable_definitions =
       [{ Graphql_ast.name = "a";
          typ = (Graphql_ast.ListType (Graphql_ast.NamedType "Bool"));
          default_value = None };
         { Graphql_ast.name = "b";
           typ =
           (Graphql_ast.ListType
              (Graphql_ast.ListType (Graphql_ast.NamedType "Int")));
           default_value = None }
         ];
       directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "x"; arguments = [];
             directives = []; selection_set = [] })
         ]
       });
  (Graphql_ast.Operation
     { Graphql_ast.optype = Graphql_ast.Query; name = (Some "NonNull");
       variable_definitions =
       [{ Graphql_ast.name = "a";
          typ = (Graphql_ast.NonNullType (Graphql_ast.NamedType "ID"));
          default_value = None };
         { Graphql_ast.name = "b";
           typ =
           (Graphql_ast.NonNullType
              (Graphql_ast.ListType (Graphql_ast.NamedType "Foo")));
           default_value = None };
         { Graphql_ast.name = "c";
           typ =
           (Graphql_ast.ListType
              (Graphql_ast.NonNullType (Graphql_ast.NamedType "Foo")));
           default_value = None };
         { Graphql_ast.name = "d";
           typ =
           (Graphql_ast.NonNullType
              (Graphql_ast.ListType
                 (Graphql_ast.NonNullType (Graphql_ast.NamedType "Foo"))));
           default_value = None }
         ];
       directives = [];
       selection_set =
       [(Graphql_ast.Field
           { Graphql_ast.alias = None; name = "x"; arguments = [];
             directives = []; selection_set = [] })
         ]
       })
  ]
