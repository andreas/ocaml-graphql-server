module Schema = Graphql_schema.Make(struct
  include Async_kernel.Deferred

  let bind x f = bind x ~f
end)
