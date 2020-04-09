module Io = struct
  type 'a t = 'a Lwt.t

  let ( >>= ) = Lwt.bind

  let return = Lwt.return

  type ic = Lwt_io.input_channel

  type oc = Lwt_io.output_channel
end

module Body = Opium_kernel.Body

module Request = struct
  type t = Opium_kernel.Rock.Request.t

  let uri (req : t) = Uri.of_string req.target

  let header ~header (req : t) =
    let headers = req.headers in
    Httpaf.Headers.get headers header

  let method_ (req : t) =
    match req.meth with `GET as m -> m | `POST as m -> m | _ -> `OTHER
end

module Response = struct
  type t = Opium_kernel.Rock.Response.t

  let make_ok ~body () =
    Opium_kernel.Rock.Response.make ~status:`OK
      ~body:(Opium_kernel.Body.of_string body)
      ()

  let make_bad_request ~body () =
    Opium_kernel.Rock.Response.make ~status:`Bad_request
      ~body:(Opium_kernel.Body.of_string body)
      ()

  let make_not_found ~body () =
    Opium_kernel.Rock.Response.make ~status:`Not_found
      ~body:(Opium_kernel.Body.of_string body)
      ()
end

module Graphql_opium =
  Graphql_server.Make (Graphql_lwt.Schema) (Io) (Request) (Response) (Body)
include Graphql_opium
