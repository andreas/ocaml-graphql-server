open Graphql

module Io = Lwt

module Stream = struct
  type +'a io = 'a Lwt.t
  type 'a t = 'a Lwt_stream.t * (unit -> unit)

  let map (stream, destroy) f =
    Lwt_stream.map_s f stream, destroy
end

module Schema = Graphql_schema.Make (Io) (Stream)

module Server = struct
  module C = Cohttp_lwt_unix
  open Lwt

  let static_file_response ?(encoding=`None) path =
    match Assets.read path with
    | Some body -> C.Server.respond_string ~status:`OK ~body ()
    | None -> C.Server.respond_string ~status:`Not_found ~body:"" ()

  let json_err = function
    | Ok _ as ok -> ok
    | Error err -> Error (`String err)

  let execute_query ctx schema variables operation_name query =
    let open Lwt_result in
    Lwt.return @@ json_err @@ Graphql_parser.parse query >>= fun doc ->
    Schema.execute schema ctx ~variables ?operation_name doc

  let execute_request ctx schema req body =
    Cohttp_lwt.Body.to_string body >>= fun body' ->
    Lwt_io.printf "Body: %s\n" body';
    let json = Yojson.Basic.from_string body' in
    let query = Yojson.Basic.(json |> Util.member "query" |> Util.to_string) in
    let variables = try Yojson.Basic.Util.(json |> member "variables" |> to_assoc) with _ -> [] in
    let operation_name =
      try Some Yojson.Basic.Util.(json |> member "operationName" |> to_string)
      with _ -> None
    in    
    Lwt_io.printf "Query: %s\n" query;
    let result = execute_query ctx schema (variables :> (string * Graphql_parser.const_value) list) operation_name query in
    result >>= function
    | Ok (`Response data) ->
        let body = Yojson.Basic.to_string data in
        C.Server.respond_string ~status:`OK ~body ()
    | Error err ->
        let body = Yojson.Basic.to_string err in
        C.Server.respond_string ~status:`Internal_server_error ~body ()

  let mk_callback mk_context schema conn (req : Cohttp.Request.t) body =
    Lwt_io.printf "Req: %s\n" req.resource;
    let req_path = Cohttp.Request.uri req |> Uri.path in
    let path_parts = Str.(split (regexp "/") req_path) in
      match req.meth, path_parts with
      | `GET,  ["graphql"]       -> static_file_response "index.html"
      | `GET,  ["graphql"; path] -> static_file_response path
      | `POST, ["graphql"]       -> execute_request (mk_context req) schema req body
      | _ -> C.Server.respond_string ~status:`Not_found ~body:"" ()

  let start ?(port=8080) ~ctx schema =
    let callback = mk_callback ctx schema in
    C.Server.create ~mode:(`TCP (`Port port)) (C.Server.make ~callback ())
end
