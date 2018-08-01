module Io = struct
  include Async_kernel.Deferred

  let bind x f = bind x ~f
end

module Stream = struct
  type +'a io = 'a Async_kernel.Deferred.t
  type 'a t = 'a Async_kernel.Pipe.Reader.t

  let map x f = Async_kernel.Pipe.map' x ~f:(fun q ->
    Async_kernel.Deferred.Queue.map q ~f)
end

module Schema = Graphql_schema.Make (Io) (Stream)
