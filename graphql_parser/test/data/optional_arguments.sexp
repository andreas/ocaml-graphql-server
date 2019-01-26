((Operation
  ((optype Query) (name (foo))
   (variable_definitions
    (((name arg) (typ (NamedType String)) (default_value ((String bar))))
     ((name other_arg) (typ (NonNullType (NamedType String)))
      (default_value ()))))
   (directives ())
   (selection_set
    ((Field
      ((alias ()) (name baz) (arguments ()) (directives ())
       (selection_set ()))))))))

