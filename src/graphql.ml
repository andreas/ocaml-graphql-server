module Schema = Graphql_schema.Make(struct
  type +'a t = 'a

  let bind x f = f x
  let return x = x
end)