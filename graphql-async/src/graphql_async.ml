module Schema = Graphql_schema.Make (struct
  include Async_kernel.Deferred

  let bind x f = bind x ~f

  module Stream = struct
    type 'a t = 'a Async_kernel.Pipe.Reader.t

    let map t f = Async_kernel.Pipe.map' t ~f:(fun q ->
      Async_kernel.Deferred.Queue.map q ~f)

    let close = Async_kernel.Pipe.close_read
  end
end)
