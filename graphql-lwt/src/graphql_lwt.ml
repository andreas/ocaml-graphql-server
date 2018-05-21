open Graphql

module Schema = Graphql_schema.Make(Lwt)

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

  let execute_query ctx schema variables query =
    let open Lwt_result in
    Lwt.return @@ json_err @@ Graphql_parser.parse query >>= fun doc ->
    Schema.execute schema ctx ~variables doc

  let execute_request ctx schema req body =
    let uri = Cohttp.Request.uri req in
    (* Use query params if the "query" query param exists; otherwise use the
       body of the request *)
    (match Uri.get_query_param uri "query" with
      | Some query ->
        Cohttp_lwt.Body.drain_body body >|= fun () ->
        let variables =
          match Uri.get_query_param uri "variables" with
          | Some json ->
            Yojson.Basic.(from_string json |> Util.to_assoc)
          | None -> []
        in
        query, variables
      | None ->
        Cohttp_lwt.Body.to_string body >|= fun body' ->
        Lwt_io.printf "Body: %s\n" body';
        let json = Yojson.Basic.from_string body' in
        let query = Yojson.Basic.(json |> Util.member "query" |> Util.to_string) in
        let variables = try Yojson.Basic.Util.(json |> member "variables" |> to_assoc) with _ -> [] in
        query, variables)
    >>= fun (query, variables) ->
    Lwt_io.printf "Query: %s\n" query;
    let result = execute_query ctx schema (variables :> (string * Graphql_parser.const_value) list) query in
    result >>= function
    | Ok data ->
        let body = Yojson.Basic.to_string data in
        C.Server.respond_string ~status:`OK ~body ()
    | Error err ->
        let body = Yojson.Basic.to_string err in
        C.Server.respond_error ~body ()

  let mk_callback mk_context schema conn (req : Cohttp.Request.t) body =
    Lwt_io.printf "Req: %s\n" req.resource;
    let req_path = Cohttp.Request.uri req |> Uri.path in
    let path_parts = Str.(split (regexp "/") req_path) in
    let accept_html =
      let headers = Cohttp.Request.headers req in
      let accept = Cohttp.Header.get headers "accept" in
      match accept with
      | None -> false
      | Some str ->
        String.split_on_char ',' str
        |> List.exists ((=) "text/html")
    in
      match req.meth, path_parts, accept_html with
      | `GET,  ["graphql"], true    -> static_file_response "index.html"
      | `GET,  ["graphql"; path], _ -> static_file_response path
      | `GET,  ["graphql"], false
      | `POST, ["graphql"], _       -> execute_request (mk_context req) schema req body
      | _ -> C.Server.respond_string ~status:`Not_found ~body:"" ()

  let start ?(port=8080) ~ctx schema =
    let callback = mk_callback ctx schema in
    C.Server.create ~mode:(`TCP (`Port port)) (C.Server.make ~callback ())
end
