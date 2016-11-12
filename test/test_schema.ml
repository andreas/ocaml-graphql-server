open Graphql

type user = {
  id   : int;
  name : string;
  role : [`user | `admin];
}

let users = [
  { id = 1; name = "Alice"; role = `admin };
  { id = 2; name = "Bob"; role = `user };
]

let role = Schema.enum
  ~name:"role"
  ~values:[(`user, "user"); (`admin, "admin")]

let user = Schema.(obj
  ~name:"user"
  ~fields:[
    field
      ~name:"id"
      ~typ:(non_null int)
      ~resolve:(fun () p -> p.id)
    ;
    field
      ~name:"name"
      ~typ:(non_null string)
      ~resolve:(fun () p -> p.name)
    ;
    field
      ~name:"role"
      ~typ:(non_null role)
      ~resolve:(fun () p -> p.role)
  ]
)

let schema = Schema.(schema 
    ~fields:[
      field
        ~name:"users"
        ~typ:(non_null (list (non_null user)))
        ~resolve:(fun () () -> users)
    ]
)
