module type HttpBody = sig
  type t
  type +'a io

  val to_string : t -> string io
  val of_string : string -> t
end

module type S = sig
  module IO : Cohttp.S.IO
  type body
  type 'ctx schema

  type response_action =
    [ `Expert of Cohttp.Response.t
                 * (IO.ic
                    -> IO.oc
                    -> unit IO.t)
    | `Response of Cohttp.Response.t * body ]

  type 'conn callback =
    'conn ->
    Cohttp.Request.t ->
    body ->
    response_action IO.t

  val execute_request :
    'ctx schema ->
    'ctx ->
    Cohttp.Request.t ->
    body ->
    response_action IO.t

  val make_callback :
    (Cohttp.Request.t -> 'ctx) ->
    'ctx schema ->
    'conn callback
end

module Make
  (Schema : Graphql_intf.Schema)
  (Io : Cohttp.S.IO with type 'a t = 'a Schema.Io.t)
  (Body : HttpBody with type +'a io := 'a Schema.Io.t) = struct

  module Ws = Websocket.Connection.Make (Io)
  module Websocket_transport = Websocket_handler.Make (Schema.Io) (Ws)

  let (>>=) = Io.(>>=)

  type response_action =
    [ `Expert of Cohttp.Response.t
                 * (Io.ic
                    -> Io.oc
                    -> unit Io.t)
    | `Response of Cohttp.Response.t * Body.t ]

  type 'conn callback =
    'conn ->
    Cohttp.Request.t ->
    Body.t ->
    response_action Io.t

  let respond_string ~status ~body () =
    Io.return (`Response (Cohttp.Response.make ~status (), Body.of_string body))

  let static_file_response path =
    match Assets.read path with
    | Some body -> respond_string ~status:`OK ~body ()
    | None -> respond_string ~status:`Not_found ~body:"" ()

  let json_err = function
    | Ok _ as ok -> ok
    | Error err -> Error (`String err)

  let execute_query ctx schema variables operation_name query =
    let parser_result = json_err (Graphql_parser.parse query) in
    Io.return parser_result >>= function
    | Ok doc -> Schema.execute schema ctx ?variables ?operation_name doc
    | Error _ as e -> Io.return e

  let execute_request schema ctx _req body =
    Body.to_string body >>= fun body' ->
    let json = Yojson.Basic.from_string body' in
    let query = Yojson.Basic.(json |> Util.member "query" |> Util.to_string) in
    let variables = Yojson.Basic.Util.(json |> member "variables" |> to_option to_assoc) in
    let operation_name = Yojson.Basic.Util.(json |> member "operationName" |> to_option to_string) in
    let result = execute_query ctx schema (variables :> (string * Graphql_parser.const_value) list option) operation_name query in
    result >>= function
    | Ok (`Response data) ->
      let body = Yojson.Basic.to_string data in
      respond_string ~status:`OK ~body ()
    | Ok (`Stream stream) ->
      Schema.Io.Stream.close stream;
      let body = "Subscriptions are only supported via websocket transport" in
      respond_string ~status:`Internal_server_error ~body ()
    | Error err ->
      let body = Yojson.Basic.to_string err in
      respond_string ~status:`Internal_server_error ~body ()

  let make_callback : (Cohttp.Request.t -> 'ctx) -> 'ctx Schema.schema -> 'conn callback = fun make_context schema _conn (req : Cohttp.Request.t) body ->
    let req_path = Cohttp.Request.uri req |> Uri.path in
    let path_parts = Astring.String.cuts ~empty:false ~sep:"/" req_path in
    match req.meth, path_parts with
    | `GET,  ["graphql"] ->
      if Cohttp.Header.get req.Cohttp.Request.headers "Connection" = Some "Upgrade" && Cohttp.Header.get req.headers "Upgrade" = Some "websocket" then
        let handle_conn =  Websocket_transport.handle (execute_query (make_context req) schema) in
        Io.return (Ws.upgrade_connection req handle_conn)
      else
        static_file_response "index.html"
    | `GET,  ["graphql"; path] -> static_file_response path
    | `POST, ["graphql"]       -> execute_request schema (make_context req) req body
    | _ -> respond_string ~status:`Not_found ~body:"" ()
end
