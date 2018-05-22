open Lwt
module C = Cohttp_lwt_unix

open Graphql_lwt

type role = User | Admin

type user = {
  id   : int;
  name : string;
  role : role;
  friends : user list;
}

let rec alice = { id = 1; name = "Alice"; role = Admin; friends = [bob] }
and bob = { id = 2; name = "Bob"; role = User; friends = [alice]}

let users = [alice; bob]

let role = Schema.(enum "role"
  ~values:[
    enum_value "USER" ~value:User ~doc:"A regular user";
    enum_value "ADMIN" ~value:Admin ~doc:"An admin user";
  ]
)

let user = Schema.(obj "user"
  ~fields:(fun user -> [
    field "id"
      ~args:Arg.[]
      ~typ:(non_null int)
      ~resolve:(fun () p -> p.id)
    ;
    field "name"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun () p -> p.name)
    ;
    field "role"
      ~args:Arg.[]
      ~typ:(non_null role)
      ~resolve:(fun () p -> p.role)
    ;
    field "friends"
      ~args:Arg.[]
      ~typ:(list (non_null user))
      ~resolve:(fun () p -> Some p.friends)
  ])
)

let schema = Schema.(schema [
    io_field "users"
      ~args:Arg.[]
      ~typ:(non_null (list (non_null user)))
      ~resolve:(fun () () -> Lwt_result.return users)
    ;
    field "greeter"
      ~typ:string
      ~args:Arg.[
        arg "config" ~typ:(non_null (obj "greeter_config" ~coerce:(fun greeting name -> (greeting, name)) ~fields:[
          arg' "greeting" ~typ:string ~default:"hello";
          arg "name" ~typ:(non_null string)
        ]))
      ]
      ~resolve:(fun () () (greeting, name) ->
        Some (Format.sprintf "%s, %s" greeting name)
      )
    ;
  ]
)

let () =
  Server.start ~ctx:(fun req -> ()) schema
  |> Lwt_main.run
