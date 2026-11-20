include
  Graphql_cohttp.Make (Graphql_lwt.Schema) (Cohttp_lwt_unix.Server.IO)
    (Cohttp_lwt.Body)
