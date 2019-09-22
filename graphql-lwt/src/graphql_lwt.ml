module Schema =
  Graphql_schema.Make
    (struct
      include Lwt

      module Stream = struct
        type 'a t = 'a Lwt_stream.t * (unit -> unit)

        let map (t, close) f = (Lwt_stream.map_s f t, close)

        let iter (t, _close) f = Lwt_stream.iter_s f t

        let close (_, close) = close ()
      end
    end)
    (struct
      type t = string

      let message_of_field_error t = t

      let extensions_of_field_error _t = None
    end)
