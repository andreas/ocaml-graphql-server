((Operation
  ((optype Query) (name (queryName))
   (variable_definitions
    (((name foo) (typ (NamedType ComplexType)) (default_value ()))
     ((name site) (typ (NamedType Site)) (default_value ((Enum MOBILE))))))
   (directives ())
   (selection_set
    ((Field
      ((alias (whoever123is)) (name node)
       (arguments ((id (List ((Int 123) (Int 456)))))) (directives ())
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
                   (arguments ((first (Int 10)) (after (Variable foo))))
                   (directives
                    (((name include) (arguments ((if (Variable foo)))))))
                   (selection_set
                    ((Field
                      ((alias ()) (name id) (arguments ()) (directives ())
                       (selection_set ())))
                     (FragmentSpread ((name frag) (directives ())))))))))))))))
         (InlineFragment
          ((type_condition ())
           (directives (((name skip) (arguments ((unless (Variable foo)))))))
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
      ((alias ()) (name like) (arguments ((story (Int 123))))
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
       (arguments ((input (Variable input)))) (directives ())
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
        ((size (Variable size)) (bar (Variable b))
         (obj (Assoc ((key (String value)))))))
       (directives ()) (selection_set ())))))))
 (Operation
  ((optype Query) (name ()) (variable_definitions ()) (directives ())
   (selection_set
    ((Field
      ((alias ()) (name unnamed)
       (arguments
        ((truthy (Bool true)) (falsey (Bool false)) (nullish Null)
         (enum (Enum ECHO))))
       (directives ()) (selection_set ())))
     (Field
      ((alias ()) (name query) (arguments ()) (directives ())
       (selection_set ()))))))))