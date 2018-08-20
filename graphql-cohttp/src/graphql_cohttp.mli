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
                          and type 'a stream = 'a Io.stream) : sig

  val start :
    ?port:int ->
    ctx:(Cohttp.Request.t -> 'a) ->
    'a Schema.schema -> unit Io.t
end
