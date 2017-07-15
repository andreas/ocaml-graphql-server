open Graphql

type role = User | Admin

type user = {
  id   : int;
  name : string;
  role : role;
}

let users = ref [
  { id = 1; name = "Alice"; role = Admin };
  { id = 2; name = "Bob"; role = User };
]

let role_values = Schema.([
  enum_value "user" ~value:User;
  enum_value "admin" ~value:Admin;
])

let role = Schema.(enum "role" ~values:role_values)
let input_role = Schema.Arg.(enum "role" ~values:role_values)

let user = Schema.(obj "user"
  ~fields:(fun _ -> [
    field "id"
      ~typ:(non_null int)
      ~args:Arg.[]
      ~resolve:(fun _ p -> p.id)
    ;
    field "name"
      ~typ:(non_null string)
      ~args:Arg.[]
      ~resolve:(fun _ p -> p.name)
    ;
    field "role"
      ~typ:(non_null role)
      ~args:Arg.[]
      ~resolve:(fun _ p -> p.role)
  ])
)

let schema : unit Schema.schema = Schema.(schema [
      field "users"
        ~typ:(non_null (list (non_null user)))
        ~args:Arg.[]
        ~resolve:(fun _ () -> !users)
    ]
    ~mutations:[
      field "add_user"
        ~typ:(non_null (list (non_null user)))
        ~args:Arg.[
          arg "name" ~typ:(non_null string);
          arg "role" ~typ:(non_null input_role)
        ]
        ~resolve:(fun _ () name role ->
          let id = Random.int 1000000 in
          users := List.append !users [{ id; name; role }];
          !users
        )
    ]
)
