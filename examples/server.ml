open Lwt
module C = Cohttp_lwt_unix

open Graphql

type user = {
  id   : int;
  name : string;
  role : [`user | `admin];
}

let users = [
  { id = 1; name = "Alice"; role = `admin };
  { id = 2; name = "Bob"; role = `user }
]

let role = Schema.enum
  ~name:"role"
  ~values:[(`user, "user"); (`admin, "admin")]

let user = Schema.(obj
  ~name:"user"
  ~fields:[
    field
      ~name:"id"
      ~args:Arg.[]
      ~typ:(non_null int)
      ~resolve:(fun () p -> p.id)
    ;
    field
      ~name:"name"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun () p -> p.name)
    ;
    field
      ~name:"role"
      ~args:Arg.[]
      ~typ:(non_null role)
      ~resolve:(fun () p -> p.role)
  ]
)

let schema = Schema.(schema 
    ~fields:[
      field
        ~name:"users"
        ~args:Arg.[]
        ~typ:(non_null (list (non_null user)))
        ~resolve:(fun () () -> users)
      ;
      field
        ~name:"greeter"
        ~typ:string
        ~args:Arg.[
          arg "config" ~typ:(non_null (obj ~name:"greeter_config" ~coerce:(fun greeting name -> (greeting, name)) ~fields:[
            arg "greeting" ~typ:(non_null string);
            arg "name" ~typ:(non_null string)
          ]))
        ]
        ~resolve:(fun () () (greeting, name) ->
          Some (Format.sprintf "%s, %s" greeting name)
        )
      ;
    ]
)

let execute query =
  let open Rresult in
  Graphql.Parser.parse query >>| fun doc ->
  Graphql.execute schema () doc

let callback conn (req : Cohttp.Request.t) body =
  Lwt_io.printf "Req: %s\n" req.resource;
  match (req.meth, req.resource) with
  | `GET, _ -> C.Server.respond_file "./index.html" ()
  | `POST, _ ->
    begin
      Cohttp_lwt_body.to_string body >>= fun query_json ->
      Lwt_io.printf "Body: %s\n" query_json;
      let query = Yojson.Basic.from_string query_json |> Yojson.Basic.Util.member "query" |> Yojson.Basic.Util.to_string in
      Lwt_io.printf "Query: %s\n" query;
      match execute query with
      | Ok data ->
          let rsp = Yojson.Basic.to_string data in
          C.Server.respond_string ~status:`OK ~body:rsp ()
      | Error err ->
          C.Server.respond_string ~status:`Internal_server_error ~body:err ()
    end
  | _ -> C.Server.respond_string ~status:`Not_found ~body:"" ()

let () =
  C.Server.create ~mode:(`TCP (`Port 8080)) (C.Server.make ~callback ())
  |> Lwt_main.run
