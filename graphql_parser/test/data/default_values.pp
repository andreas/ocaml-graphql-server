[(Graphql_ast.Operation
    { Graphql_ast.optype = Graphql_ast.Query; name = (Some "DefaultValues");
      variable_definitions =
      [{ Graphql_ast.name = "a"; typ = (Graphql_ast.NamedType "Int");
         default_value = (Some `Int (1)) };
        { Graphql_ast.name = "b";
          typ = (Graphql_ast.ListType (Graphql_ast.NamedType "String"));
          default_value = (Some `List ([`String ("a")])) };
        { Graphql_ast.name = "c"; typ = (Graphql_ast.NamedType "Obj");
          default_value =
          (Some `Assoc ([("x", `Assoc ([("y", `Bool (true))]));
                          ("z", `Enum ("RED"))]))
          };
        { Graphql_ast.name = "d";
          typ =
          (Graphql_ast.NonNullType
             (Graphql_ast.ListType (Graphql_ast.NamedType "Obj")));
          default_value =
          (Some `List ([`Assoc ([("x", `Assoc ([("y", `Null)]));
                                  ("z", `Enum ("BLUE"))])
                         ]))
          }
        ];
      directives = [];
      selection_set =
      [(Graphql_ast.Field
          { Graphql_ast.alias = None; name = "x"; arguments = [];
            directives = []; selection_set = [] })
        ]
      })
  ]
