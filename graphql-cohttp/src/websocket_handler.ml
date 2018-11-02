module Make (IO : Graphql_intf.IO) (Ws : Websocket.Connection.S with type 'a IO.t = 'a IO.t) = struct
  module Json = Yojson.Basic.Util

  let (>>=) = IO.bind

  type t = {
    conn : Ws.t;
    subscriptions : (string, unit -> unit) Hashtbl.t;
  }

  type client_message =
    | Gql_connection_init
    | Gql_start of {
        id : string;
        query : string;
        variables : (string * Graphql_parser.const_value) list option;
        operation_name : string option;
      }
    | Gql_stop of {
        id : string;
      }
    | Gql_connection_terminate

  type server_message =
    | Gql_connection_error
    | Gql_connection_ack
    | Gql_data
    | Gql_error
    | Gql_complete
 (* | Gql_connection_keep_alive *)

    let client_message_of_payload msg =
      match msg |> Json.member "type" |> Json.to_string_option with
      | Some "connection_init" ->
          Ok Gql_connection_init
      | Some "start" ->
          let id = Json.(msg |> member "id" |> to_string) in
          let payload = Json.member "payload" msg in
          let query = Json.(payload |> member "query" |> to_string) in
          let variables = Json.(payload |> member "variables" |> to_option to_assoc) in
          let variables = (variables :> (string * Graphql_parser.const_value) list option) in
          let operation_name = Json.(payload |> member "operationName" |> to_string_option)
          in
          Ok (Gql_start { id; query; variables; operation_name })
      | Some "stop" ->
          let id = Json.(member "id" msg |> to_string) in
          Ok (Gql_stop { id })
      | Some "connection_terminate" ->
          Ok Gql_connection_terminate
      | Some typ ->
          Error (Format.sprintf "Unknown message type `%s`" typ)
      | None ->
          Error (Format.sprintf "Missing message type")

    let server_message_to_string = function
      | Gql_connection_error -> "connection_error"
      | Gql_connection_ack -> "connection_ack"
      | Gql_data -> "data"
      | Gql_error -> "error"
      | Gql_complete -> "complete"
    (* | Gql_connection_keep_alive -> "ka" *)

    let create_message ?(opcode=Websocket.Frame.Opcode.Text) ?id ?(payload=`Null) typ =
      let frame_payload = `Assoc [
          "type", `String (server_message_to_string typ);
          "id", begin match id with
            | Some id -> `String id
            | None -> `Null end;
          "payload", payload
        ] in
      let content = Yojson.Basic.to_string frame_payload in
      Websocket.Frame.create ~opcode ~content ()

    let handle_frame t ~execute_query frame =
      match frame.Websocket.Frame.opcode with
      | Ping
      | Pong
      | Close
      | Ctrl _
      | Nonctrl _ ->
        IO.return ()
      | Continuation
      | Text
      | Binary ->
        let json = Yojson.Basic.from_string frame.Websocket.Frame.content in
        match client_message_of_payload json with
        | Ok Gql_connection_init ->
          Ws.send t.conn (create_message Gql_connection_ack);
        | Ok (Gql_start { id; query; variables; operation_name }) ->
          execute_query
            variables
            operation_name
            query
          >>= (function
          | Error message ->
            let payload = `Assoc ["message", message] in
            Ws.send t.conn (create_message ~payload ~id Gql_error)
          | Ok (`Response payload) ->
            Ws.send t.conn (create_message ~id ~payload Gql_data)
          | Ok (`Stream stream) ->
            let close () = IO.Stream.close stream in
            Hashtbl.add t.subscriptions id close;
            IO.Stream.iter stream (fun response ->
              let Ok payload | Error payload = response in
              Ws.send t.conn (create_message ~id ~payload Gql_data)
            ) >>= fun () ->
            Ws.send t.conn (create_message ~id Gql_complete)
          )
        | Ok (Gql_stop { id }) ->
          begin try
            let close = Hashtbl.find t.subscriptions id in
            close ()
          with Not_found -> ()
          end;
          IO.return ()
        | Ok Gql_connection_terminate ->
          Hashtbl.iter (fun _id close -> close ()) t.subscriptions;
          Ws.send t.conn (create_message ~opcode:Websocket.Frame.Opcode.Close Gql_connection_error)
        | Error msg ->
          let id = Json.(json |> member "id" |> to_string) in
          let payload = `Assoc ["message", `String msg] in
          Ws.send t.conn (create_message ~id ~payload Gql_error)

    let handle execute_query conn =
      let subscriptions = Hashtbl.create 8 in
      let t = { conn; subscriptions } in
      let rec loop () =
        Ws.recv conn >>= fun frame ->
        handle_frame t ~execute_query frame >>= fun () ->
        loop ()
      in
      loop ()
end
