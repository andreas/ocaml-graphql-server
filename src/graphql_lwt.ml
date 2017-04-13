module Schema = Graphql_schema.Make(struct
  type +'a t = 'a Lwt.t

  let return = Lwt.return
  let bind x ~f = Lwt.bind x f
end)
