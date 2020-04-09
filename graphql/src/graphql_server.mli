module type Io = sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t
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

module Make
    (Schema : Graphql_intf.Schema)
    (Io : Io with type 'a t = 'a Schema.Io.t)
    (Request : HttpRequest)
    (Response : HttpResponse)
    (Body : HttpBody with type +'a io := 'a Schema.Io.t) :
  S
    with type 'ctx schema := 'ctx Schema.schema
     and module Io := Io
     and type body := Body.t
     and type request := Request.t
     and type response := Response.t
     and type callback := Request.t -> Body.t -> Response.t Io.t
