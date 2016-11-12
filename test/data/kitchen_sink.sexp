((Operation
  ((optype Query) (name (queryName))
   (variable_definitions
    (((name foo) (typ (NamedType ComplexType)) (default_value ()))
     ((name site) (typ (NamedType Site)) (default_value ((Enum MOBILE))))))
   (directives ())
   (selection_set
    ((Field
      ((alias (whoever123is)) (name node)
       (arguments (((name id) (value (List ((Int 123) (Int 456)))))))
       (directives ())
       (selection_set
        ((Field
          ((alias ()) (name id) (arguments ()) (directives ())
           (selection_set ())))
         (InlineFragment
          ((type_condition (User))
           (directives (((name defer) (arguments ()))))
           (selection_set
            ((Field
              ((alias ()) (name field2) (arguments ()) (directives ())
               (selection_set
                ((Field
                  ((alias ()) (name id) (arguments ()) (directives ())
                   (selection_set ())))
                 (Field
                  ((alias (alias)) (name field1)
                   (arguments
                    (((name first) (value (Int 10)))
                     ((name after) (value (Variable foo)))))
                   (directives
                    (((name include)
                      (arguments (((name if) (value (Variable foo))))))))
                   (selection_set
                    ((Field
                      ((alias ()) (name id) (arguments ()) (directives ())
                       (selection_set ())))
                     (FragmentSpread ((name frag) (directives ())))))))))))))))
         (InlineFragment
          ((type_condition ())
           (directives
            (((name skip)
              (arguments (((name unless) (value (Variable foo))))))))
           (selection_set
            ((Field
              ((alias ()) (name id) (arguments ()) (directives ())
               (selection_set ())))))))
         (InlineFragment
          ((type_condition ()) (directives ())
           (selection_set
            ((Field
              ((alias ()) (name id) (arguments ()) (directives ())
               (selection_set ())))))))))))))))
 (Operation
  ((optype Mutation) (name (likeStory)) (variable_definitions ())
   (directives ())
   (selection_set
    ((Field
      ((alias ()) (name like) (arguments (((name story) (value (Int 123)))))
       (directives (((name defer) (arguments ()))))
       (selection_set
        ((Field
          ((alias ()) (name story) (arguments ()) (directives ())
           (selection_set
            ((Field
              ((alias ()) (name id) (arguments ()) (directives ())
               (selection_set ())))))))))))))))
 (Operation
  ((optype Subscription) (name (StoryLikeSubscription))
   (variable_definitions
    (((name input) (typ (NamedType StoryLikeSubscribeInput))
      (default_value ()))))
   (directives ())
   (selection_set
    ((Field
      ((alias ()) (name storyLikeSubscribe)
       (arguments (((name input) (value (Variable input))))) (directives ())
       (selection_set
        ((Field
          ((alias ()) (name story) (arguments ()) (directives ())
           (selection_set
            ((Field
              ((alias ()) (name likers) (arguments ()) (directives ())
               (selection_set
                ((Field
                  ((alias ()) (name count) (arguments ()) (directives ())
                   (selection_set ())))))))
             (Field
              ((alias ()) (name likeSentence) (arguments ()) (directives ())
               (selection_set
                ((Field
                  ((alias ()) (name text) (arguments ()) (directives ())
                   (selection_set ())))))))))))))))))))
 (Fragment
  ((name frag) (type_condition Friend) (directives ())
   (selection_set
    ((Field
      ((alias ()) (name foo)
       (arguments
        (((name size) (value (Variable size)))
         ((name bar) (value (Variable b)))
         ((name obj) (value (Object (((name key) (value (String value)))))))))
       (directives ()) (selection_set ()))))))))
