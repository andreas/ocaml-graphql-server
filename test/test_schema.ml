open Graphql

type role = User | Admin

type user = {
  id   : int;
  name : string;
  role : role;
}

let users = [
  { id = 1; name = "Alice"; role = Admin };
  { id = 2; name = "Bob"; role = User };
]

let role = Schema.enum "role"
  ~values:[(User, "user"); (Admin, "admin")]

let user = Schema.(obj "user"
  ~fields:(fun _ -> [
    field "id"
      ~typ:(non_null int)
      ~args:Arg.[]
      ~resolve:(fun () p -> p.id)
    ;
    field "name"
      ~typ:(non_null string)
      ~args:Arg.[]
      ~resolve:(fun () p -> p.name)
    ;
    field "role"
      ~typ:(non_null role)
      ~args:Arg.[]
      ~resolve:(fun () p -> p.role)
  ])
)

let schema = Schema.(schema 
    ~fields:[
      field "users"
        ~typ:(non_null (list (non_null user)))
        ~args:Arg.[]
        ~resolve:(fun () () -> users)
    ]
)
