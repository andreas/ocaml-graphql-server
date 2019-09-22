module Schema =
  Graphql_schema.Make
    (struct
      include Async_kernel.Deferred

      let bind x f = bind x ~f

      module Stream = struct
        type 'a t = 'a Async_kernel.Pipe.Reader.t

        let map t f =
          Async_kernel.Pipe.map' t ~f:(fun q ->
              Async_kernel.Deferred.Queue.map q ~f)

        let iter t f = Async_kernel.Pipe.iter t ~f

        let close = Async_kernel.Pipe.close_read
      end
    end)
    (struct
      type t = string

      let message_of_field_error t = t

      let extensions_of_field_error _t = None
    end)
