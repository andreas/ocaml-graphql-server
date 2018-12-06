module type HttpBody = sig
  type t
  type +'a io

  val to_string : t -> string io
  val of_string : string -> t
end

module Make
  (Schema : Graphql_intf.Schema)
  (Body : HttpBody with type +'a io := 'a Schema.Io.t) : sig

  type 'conn callback =
    'conn ->
    Cohttp.Request.t ->
    Body.t ->
    (Cohttp.Response.t * Body.t) Schema.Io.t

  val execute_request :
    'ctx Schema.schema ->
    'ctx ->
    Cohttp.Request.t ->
    Body.t ->
    (Cohttp.Response.t * Body.t) Schema.Io.t

  val make_callback :
    (Cohttp.Request.t -> 'ctx) ->
    'ctx Schema.schema ->
    'conn callback
end
