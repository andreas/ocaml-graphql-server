module type IO = sig
  type +'a t
  type 'a stream

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type Server = sig
  type +'a io
  type t
  type conn

  type server

  val tcp_of_port : int -> server
  module Body : sig
    type t

    val to_string : t -> string io
  end

  val create :
    ?mode:server -> t -> unit io

  val make :
    callback:(conn -> Cohttp.Request.t -> Body.t
              -> (Cohttp.Response.t * Body.t) io)
    -> unit -> t

  val respond_string :
    ?flush:bool ->
    ?headers:Cohttp.Header.t ->
    status:Cohttp.Code.status_code ->
    body:string -> unit -> (Cohttp.Response.t * Body.t) io
end

module Make
    (Io : IO)
    (Server : Server with type +'a io = 'a Io.t)
    (Schema : Graphql_intf.Schema with type +'a io = 'a Io.t
                                   and type 'a stream = 'a Io.stream) = struct

  module Io = struct
    include Io

    module Infix = struct
      let (>>=) = Io.bind
    end
  end

  let static_file_response ?(encoding=`None) path =
    match Assets.read path with
    | Some body -> Server.respond_string ~status:`OK ~body ()
    | None -> Server.respond_string ~status:`Not_found ~body:"" ()

  let json_err = function
    | Ok _ as ok -> ok
    | Error err -> Error (`String err)

  let execute_query ctx schema variables operation_name query =
    let open Io.Infix in
    let parser_result = json_err (Graphql_parser.parse query) in
    Io.return parser_result >>= function
    | Ok doc -> Schema.execute schema ctx ~variables ?operation_name doc
    | Error _ as e -> Io.return e

  let execute_request ctx schema req body =
    let open Io.Infix in
    Server.Body.to_string body >>= fun body' ->
    Printf.printf "Body: %s\n%!" body';
    let json = Yojson.Basic.from_string body' in
    let query = Yojson.Basic.(json |> Util.member "query" |> Util.to_string) in
    let variables = try Yojson.Basic.Util.(json |> member "variables" |> to_assoc) with _ -> [] in
    let operation_name =
      try Some Yojson.Basic.Util.(json |> member "operationName" |> to_string)
      with _ -> None
    in
    Printf.printf "Query: %s\n%!" query;
    let result = execute_query ctx schema (variables :> (string * Graphql_parser.const_value) list) operation_name query in
    result >>= function
    | Ok (`Response data) ->
      let body = Yojson.Basic.to_string data in
      Server.respond_string ~status:`OK ~body ()
    | Error err ->
      let body = Yojson.Basic.to_string err in
      Server.respond_string ~status:`Internal_server_error ~body ()

  let mk_callback mk_context schema conn (req : Cohttp.Request.t) body =
    Printf.printf "Req: %s\n%!" req.resource;
    let req_path = Cohttp.Request.uri req |> Uri.path in
    let path_parts = Str.(split (regexp "/") req_path) in
    match req.meth, path_parts with
    | `GET,  ["graphql"]       -> static_file_response "index.html"
    | `GET,  ["graphql"; path] -> static_file_response path
    | `POST, ["graphql"]       -> execute_request (mk_context req) schema req body
    | _ -> Server.respond_string ~status:`Not_found ~body:"" ()

  let start ?(port=8080) ~ctx schema =
    let callback = mk_callback ctx schema in
    Server.create ~mode:(Server.tcp_of_port port) (Server.make ~callback ())
end
