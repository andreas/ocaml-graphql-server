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
  (IO : Cohttp.S.IO with type 'a t = 'a Schema.Io.t)
  (Body : HttpBody with type +'a io := 'a Schema.Io.t) :
  S with type 'ctx schema := 'ctx Schema.schema
     and module IO := IO
     and type body := Body.t
