open Lwt.Infix

let executable_query (query, kvariables, parse) =
  fun ~token -> (
    kvariables (fun variables ->
        let uri = Uri.of_string "https://api.github.com/graphql" in
        let headers = Cohttp.Header.of_list [
          "Authorization", "bearer " ^ token;
          "User-Agent", "andreas/ocaml-graphql";
        ] in
        let body = `Assoc [
          "query", `String query;
          "variables", variables;
        ] in
        let serialized_body = Yojson.Basic.to_string body in
        Cohttp_lwt_unix.Client.post ~headers ~body:(`String serialized_body) uri >>= fun (rsp, body) ->
        Cohttp_lwt_body.to_string body >|= fun body' ->
        match Cohttp.Code.(code_of_status rsp.status |> is_success) with
        | false ->
            Error body'
        | true ->
            try
              Ok (Yojson.Basic.from_string body' |> parse)
            with Yojson.Json_error err ->
              Error err
    )
  )

let find_repository = executable_query [%graphql {|
  query FindRepository($owner: String!, $name: String!) {
    repository(owner: $owner, name: $name) {
      id
      description
    }
  }
|}]

let main () =
  let token = Unix.getenv "GITHUB_TOKEN" in
  find_repository ~token ~owner:"andreas" ~name:"ocaml-graphql-server" () >|= function
  | Ok rsp ->
      begin match rsp#repository with
      | Some repo ->
          Format.printf "Repo id: %s" repo#id
      | None ->
          Format.printf "Repo not found"
      end
  | Error err ->
      Format.printf "Error! %s" err

let () =
  Lwt_main.run (main ())