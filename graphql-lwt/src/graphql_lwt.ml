module Io = Lwt

module Stream = struct
  type +'a io = 'a Lwt.t
  type 'a t = 'a Lwt_stream.t * (unit -> unit)

  let map (stream, destroy) f =
    Lwt_stream.map_s f stream, destroy
end

module Schema = Graphql_schema.Make (Io) (Stream)
