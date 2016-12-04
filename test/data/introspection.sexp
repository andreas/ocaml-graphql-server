((Operation
  ((optype Query) (name (IntrospectionQuery)) (variable_definitions ())
   (directives ())
   (selection_set
    ((Field
      ((alias ()) (name __schema) (arguments ()) (directives ())
       (selection_set
        ((Field
          ((alias ()) (name queryType) (arguments ()) (directives ())
           (selection_set
            ((Field
              ((alias ()) (name name) (arguments ()) (directives ())
               (selection_set ())))))))
         (Field
          ((alias ()) (name mutationType) (arguments ()) (directives ())
           (selection_set
            ((Field
              ((alias ()) (name name) (arguments ()) (directives ())
               (selection_set ())))))))
         (Field
          ((alias ()) (name subscriptionType) (arguments ()) (directives ())
           (selection_set
            ((Field
              ((alias ()) (name name) (arguments ()) (directives ())
               (selection_set ())))))))
         (Field
          ((alias ()) (name types) (arguments ()) (directives ())
           (selection_set
            ((FragmentSpread ((name FullType) (directives ())))))))
         (Field
          ((alias ()) (name directives) (arguments ()) (directives ())
           (selection_set
            ((Field
              ((alias ()) (name name) (arguments ()) (directives ())
               (selection_set ())))
             (Field
              ((alias ()) (name description) (arguments ()) (directives ())
               (selection_set ())))
             (Field
              ((alias ()) (name args) (arguments ()) (directives ())
               (selection_set
                ((FragmentSpread ((name InputValue) (directives ())))))))
             (Field
              ((alias ()) (name onOperation) (arguments ()) (directives ())
               (selection_set ())))
             (Field
              ((alias ()) (name onFragment) (arguments ()) (directives ())
               (selection_set ())))
             (Field
              ((alias ()) (name onField) (arguments ()) (directives ())
               (selection_set ())))))))))))))))
 (Fragment
  ((name FullType) (type_condition __Type) (directives ())
   (selection_set
    ((Field
      ((alias ()) (name kind) (arguments ()) (directives ())
       (selection_set ())))
     (Field
      ((alias ()) (name name) (arguments ()) (directives ())
       (selection_set ())))
     (Field
      ((alias ()) (name description) (arguments ()) (directives ())
       (selection_set ())))
     (Field
      ((alias ()) (name fields)
       (arguments ((includeDeprecated (Boolean true)))) (directives ())
       (selection_set
        ((Field
          ((alias ()) (name name) (arguments ()) (directives ())
           (selection_set ())))
         (Field
          ((alias ()) (name description) (arguments ()) (directives ())
           (selection_set ())))
         (Field
          ((alias ()) (name args) (arguments ()) (directives ())
           (selection_set
            ((FragmentSpread ((name InputValue) (directives ())))))))
         (Field
          ((alias ()) (name type) (arguments ()) (directives ())
           (selection_set
            ((FragmentSpread ((name TypeRef) (directives ())))))))
         (Field
          ((alias ()) (name isDeprecated) (arguments ()) (directives ())
           (selection_set ())))
         (Field
          ((alias ()) (name deprecationReason) (arguments ()) (directives ())
           (selection_set ())))))))
     (Field
      ((alias ()) (name inputFields) (arguments ()) (directives ())
       (selection_set ((FragmentSpread ((name InputValue) (directives ())))))))
     (Field
      ((alias ()) (name interfaces) (arguments ()) (directives ())
       (selection_set ((FragmentSpread ((name TypeRef) (directives ())))))))
     (Field
      ((alias ()) (name enumValues)
       (arguments ((includeDeprecated (Boolean true)))) (directives ())
       (selection_set
        ((Field
          ((alias ()) (name name) (arguments ()) (directives ())
           (selection_set ())))
         (Field
          ((alias ()) (name description) (arguments ()) (directives ())
           (selection_set ())))
         (Field
          ((alias ()) (name isDeprecated) (arguments ()) (directives ())
           (selection_set ())))
         (Field
          ((alias ()) (name deprecationReason) (arguments ()) (directives ())
           (selection_set ())))))))
     (Field
      ((alias ()) (name possibleTypes) (arguments ()) (directives ())
       (selection_set ((FragmentSpread ((name TypeRef) (directives ())))))))))))
 (Fragment
  ((name InputValue) (type_condition __InputValue) (directives ())
   (selection_set
    ((Field
      ((alias ()) (name name) (arguments ()) (directives ())
       (selection_set ())))
     (Field
      ((alias ()) (name description) (arguments ()) (directives ())
       (selection_set ())))
     (Field
      ((alias ()) (name type) (arguments ()) (directives ())
       (selection_set ((FragmentSpread ((name TypeRef) (directives ())))))))
     (Field
      ((alias ()) (name defaultValue) (arguments ()) (directives ())
       (selection_set ())))))))
 (Fragment
  ((name TypeRef) (type_condition __Type) (directives ())
   (selection_set
    ((Field
      ((alias ()) (name kind) (arguments ()) (directives ())
       (selection_set ())))
     (Field
      ((alias ()) (name name) (arguments ()) (directives ())
       (selection_set ())))
     (Field
      ((alias ()) (name ofType) (arguments ()) (directives ())
       (selection_set
        ((Field
          ((alias ()) (name kind) (arguments ()) (directives ())
           (selection_set ())))
         (Field
          ((alias ()) (name name) (arguments ()) (directives ())
           (selection_set ())))
         (Field
          ((alias ()) (name ofType) (arguments ()) (directives ())
           (selection_set
            ((Field
              ((alias ()) (name kind) (arguments ()) (directives ())
               (selection_set ())))
             (Field
              ((alias ()) (name name) (arguments ()) (directives ())
               (selection_set ())))
             (Field
              ((alias ()) (name ofType) (arguments ()) (directives ())
               (selection_set
                ((Field
                  ((alias ()) (name kind) (arguments ()) (directives ())
                   (selection_set ())))
                 (Field
                  ((alias ()) (name name) (arguments ()) (directives ())
                   (selection_set ()))))))))))))))))))))