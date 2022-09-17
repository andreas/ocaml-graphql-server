open Eio.Std

module Eio_io = struct
  type +'a t = 'a Eio.Promise.t

  let return = Promise.create_resolved

  let bind : 'a Promise.t -> ('a -> 'b Promise.t) -> 'b Promise.t =
   fun p f -> f (Promise.await p)

  module Stream = struct
    type 'a t = 'a Seq.t

    let map t f = Seq.map (fun x -> Promise.await (f x)) t

    let iter t f =
      Promise.create_resolved (Seq.iter (fun x -> Promise.await (f x)) t)

    let close _t = ()
  end
end

module Schema =
  Graphql_schema.Make
    (Eio_io)
    (struct
      type t = string

      let message_of_field_error t = t
      let extensions_of_field_error _t = None
    end)
