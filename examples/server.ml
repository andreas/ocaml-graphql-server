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

let rec consume_stream stream =
  Lwt.catch (fun () ->
    Lwt_stream.next stream >>= fun x ->
      let Ok x | Error x = x in
      Printf.eprintf "stream response: '%s'\n%!" (Yojson.Basic.to_string x);
    if Lwt_stream.is_closed stream then
      Lwt.return_unit
    else
      consume_stream stream)
  (function
    | Lwt_stream.Closed | Lwt_stream.Empty -> Lwt.return_unit
   | _ -> Lwt.return_unit)

let set_interval s f destroy =
  let rec set_interval_loop s f n =
    let timeout = Lwt_timeout.create s (fun () ->
      if n > 0 then begin
      f ();
      set_interval_loop s f (n - 1)
      end else
        destroy ())
    in
    Lwt_timeout.start timeout
  in
  set_interval_loop s f 5

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
  ~subscriptions:[
      subscription_field "subscribe_to_user"
        ~typ:(non_null user)
        ~args:Arg.[arg' "intarg" ~typ:int ~default:42]
        ~resolve:(fun _ctx _intarg ->
          let user_stream, push_to_user_stream = Lwt_stream.create () in
          let destroy_stream = (fun () -> push_to_user_stream None) in
          set_interval 2 (fun () ->
            let idx = Random.int (List.length users) in
            push_to_user_stream (Some (List.nth users idx)))
            destroy_stream;
          Lwt_result.return (user_stream, destroy_stream))
    ]
)

let () =
  Server.start ~ctx:(fun req -> ()) schema
  |> Lwt_main.run
