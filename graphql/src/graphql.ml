module Io = struct
  type +'a t = 'a

  let bind x f = f x
  let return x = x
end

module Stream = struct
  type +'a io = 'a Io.t
  type 'a t = 'a Seq.t

  let map x f = Seq.map f x
end

module Schema = Graphql_schema.Make (Io) (Stream)
