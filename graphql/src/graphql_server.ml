module type Io = sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  type ic

  type oc
end

module type HttpBody = sig
  type t

  type +'a io

  val to_string : t -> string io

  val of_string : string -> t
end

module type HttpRequest = sig
  type t

  val uri : t -> Uri.t

  val header : header:string -> t -> string option

  val method_ : t -> [ `GET | `POST | `OTHER ]
end

module type HttpResponse = sig
  type t

  val make_ok : body:string -> unit -> t

  val make_bad_request : body:string -> unit -> t

  val make_not_found : body:string -> unit -> t
end

module type S = sig
  module Io : Io

  type body

  type request

  type response

  type 'ctx schema

  type callback

  val execute_request : 'ctx schema -> 'ctx -> callback

  val make_callback : (request -> 'ctx) -> 'ctx schema -> callback
end

module Option = struct
  let bind t ~f = match t with None -> None | Some x -> f x

  let map t ~f = bind t ~f:(fun x -> Some (f x))

  let first_some t t' = match t with None -> t' | Some _ -> t
end

module Params (Request : HttpRequest) = struct
  type t =
    { query : string option
    ; variables : (string * Yojson.Basic.t) list option
    ; operation_name : string option
    }

  let empty = { query = None; variables = None; operation_name = None }

  let of_uri_exn uri =
    let variables =
      Uri.get_query_param uri "variables"
      |> Option.map ~f:Yojson.Basic.from_string
      |> Option.map ~f:Yojson.Basic.Util.to_assoc
    in
    { query = Uri.get_query_param uri "query"
    ; variables
    ; operation_name = Uri.get_query_param uri "operationName"
    }

  let of_json_body_exn body =
    if body = "" then
      empty
    else
      let json = Yojson.Basic.from_string body in
      { query =
          Yojson.Basic.Util.(json |> member "query" |> to_option to_string)
      ; variables =
          Yojson.Basic.Util.(json |> member "variables" |> to_option to_assoc)
      ; operation_name =
          Yojson.Basic.Util.(
            json |> member "operationName" |> to_option to_string)
      }

  let of_graphql_body body =
    { query = Some body; variables = None; operation_name = None }

  let merge t t' =
    { query = Option.first_some t.query t'.query
    ; variables = Option.first_some t.variables t'.variables
    ; operation_name = Option.first_some t.operation_name t'.operation_name
    }

  let post_params_exn req body =
    let header = Request.header req ~header:"Content-Type" in
    match header with
    | Some "application/graphql" ->
      of_graphql_body body
    | Some "application/json" ->
      of_json_body_exn body
    | _ ->
      empty

  let of_req_exn req body =
    let uri = Request.uri req in
    let get_params = of_uri_exn uri in
    let post_params = post_params_exn req body in
    merge get_params post_params

  let extract req body =
    try
      let params = of_req_exn req body in
      match params.query with
      | Some query ->
        Ok
          ( query
          , (params.variables
              :> (string * Graphql_parser.const_value) list option)
          , params.operation_name )
      | None ->
        Error "Must provide query string"
    with
    | Yojson.Json_error msg ->
      Error msg
end

module Make
    (Schema : Graphql_intf.Schema)
    (Io : Io with type 'a t = 'a Schema.Io.t)
    (Request : HttpRequest)
    (Response : HttpResponse)
    (Body : HttpBody with type +'a io := 'a Schema.Io.t) =
struct
  module Params = Params (Request)

  let ( >>= ) = Io.( >>= )

  let execute_query ctx schema variables operation_name query =
    match Graphql_parser.parse query with
    | Ok doc ->
      Schema.execute schema ctx ?variables ?operation_name doc
    | Error e ->
      Schema.Io.return (Error (`String e))

  let execute_request schema ctx req body =
    Body.to_string body >>= fun body_string ->
    match Params.extract req body_string with
    | Error err ->
      Io.return @@ Response.make_bad_request ~body:err ()
    | Ok (query, variables, operation_name) ->
      execute_query ctx schema variables operation_name query >>= ( function
      | Ok (`Response data) ->
        let body = Yojson.Basic.to_string data in
        Io.return @@ Response.make_ok ~body ()
      | Ok (`Stream stream) ->
        Schema.Io.Stream.close stream;
        let body = "Subscriptions are only supported via websocket transport" in
        Io.return @@ Response.make_bad_request ~body ()
      | Error err ->
        let body = Yojson.Basic.to_string err in
        Io.return @@ Response.make_bad_request ~body () )

  let make_callback make_context schema req body =
    let method_ = Request.method_ req in
    match method_ with
    | `GET ->
      if
        Request.header req ~header:"Connection" = Some "Upgrade"
        && Request.header req ~header:"Upgrade" = Some "websocket"
      then
        (* let handle_conn = Websocket_transport.handle (execute_query
           (make_context req) schema) in Io.return (Ws.upgrade_connection req
           handle_conn) *)
        assert false
      else
        execute_request schema (make_context req) req body
    | `POST ->
      execute_request schema (make_context req) req body
    | _ ->
      Io.return @@ Response.make_not_found ~body:"Not found" ()
end
