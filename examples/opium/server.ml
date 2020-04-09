open Opium.Std

let graphql =
  get "/" (fun req ->
      let schema = Schema.schema in
      let callback = Graphql_opium.make_callback (fun _req -> ()) schema in
      callback req req.body)

let _ = App.empty |> graphql |> App.run_command
