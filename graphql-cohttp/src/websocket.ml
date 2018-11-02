(*
 * Copyright (c) 2012-2018 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Astring

let b64_encoded_sha1sum s =
  s
  |> Digestif.SHA1.digest_string
  |> Digestif.SHA1.to_raw_string
  |> Base64.encode_exn ~pad:true

let websocket_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

module Frame = struct
  module Opcode = struct
    type t =
      | Continuation
      | Text
      | Binary
      | Close
      | Ping
      | Pong
      | Ctrl of int
      | Nonctrl of int

    let to_string = function
    | Continuation -> "continuation"
    | Text -> "text"
    | Binary -> "binary"
    | Close -> "close"
    | Ping -> "ping"
    | Pong -> "pong"
    | Ctrl i -> "ctrl " ^ string_of_int i
    | Nonctrl i -> "nonctrl " ^ string_of_int i

    let pp ppf t = Format.fprintf ppf "%s" (to_string t)

    let of_enum = function
      | i when (i < 0 || i > 0xf) -> invalid_arg "Frame.Opcode.of_enum"
      | 0                         -> Continuation
      | 1                         -> Text
      | 2                         -> Binary
      | 8                         -> Close
      | 9                         -> Ping
      | 10                        -> Pong
      | i when i < 8              -> Nonctrl i
      | i                         -> Ctrl i

    let to_enum = function
      | Continuation   -> 0
      | Text           -> 1
      | Binary         -> 2
      | Close          -> 8
      | Ping           -> 9
      | Pong           -> 10
      | Ctrl i         -> i
      | Nonctrl i      -> i

    let is_ctrl opcode = to_enum opcode > 7
  end

  type t = {
    opcode: Opcode.t;
    extension: int;
    final: bool;
    content: string;
  }

  let pp ppf { opcode ; extension ; final ; content } =
    Format.fprintf ppf
      "[%a (0x%x) (final=%b) %s]" Opcode.pp opcode extension final content

  let show t = Format.asprintf "%a" pp t

  let create
      ?(opcode = Opcode.Text) ?(extension=0) ?(final=true) ?(content="") () =
    { opcode ; extension ; final ; content }

  let of_bytes ?opcode ?extension ?final content =
    let content = Bytes.unsafe_to_string content in
    create ?opcode ?extension ?final ~content ()

  let close code =
    let content = Bytes.create 2 in
    EndianBytes.BigEndian.set_int16 content 0 code;
    of_bytes ~opcode:Opcode.Close content
end

module Bits = struct
  let xor mask msg =
    for i = 0 to Bytes.length msg - 1 do (* masking msg to send *)
      Bytes.set msg i Char.(to_int mask.[i mod 4] lxor to_int (Bytes.get msg i) |> of_byte)
    done

  let is_bit_set idx v =
    (v lsr idx) land 1 = 1

  let set_bit v idx b =
    if b then v lor (1 lsl idx) else v land (lnot (1 lsl idx))

  let int_value shift len v = (v lsr shift) land ((1 lsl len) - 1)
end

exception Protocol_error of string

module Connection = struct
  type mode =
    | Client of (int -> string)
    | Server

  module type S = sig
    module IO : Cohttp.S.IO
    type t

    val create :
      ?read_buf:Buffer.t ->
      ?write_buf:Buffer.t ->
      mode:mode ->
      Cohttp.Request.t ->
      IO.ic ->
      IO.oc ->
      t

    val send : t -> Frame.t -> unit IO.t
    val send_multiple : t -> Frame.t list -> unit IO.t
    val recv : t -> Frame.t IO.t
    val req : t -> Cohttp.Request.t

    val upgrade_connection :
      ?read_buf : Buffer.t ->
      ?write_buf : Buffer.t ->
      Cohttp.Request.t ->
      (t -> unit IO.t) ->
      [> `Expert of Cohttp.Response.t * (IO.ic -> IO.oc -> unit IO.t) ]
  end

  module Make (IO : Cohttp.S.IO) = struct
    module IO = IO
    let (>>=) = IO.(>>=)

    let is_client mode = mode <> Server

    let rec read_exactly ic remaining buf =
      IO.read ic remaining >>= fun s ->
      if s = "" then
        IO.return None
      else
        let recv_len = String.length s in
        Buffer.add_string buf s;
        if remaining - recv_len <= 0 then
          IO.return @@ Some (Buffer.contents buf)
        else
          read_exactly ic (remaining - recv_len) buf

    let read_uint16 ic buf =
      read_exactly ic 2 buf >>= fun s ->
      match s with
      | None ->
          IO.return None
      | Some s ->
          IO.return @@ Some (EndianString.BigEndian.get_uint16 s 0)

    let read_int64 ic buf =
      read_exactly ic 8 buf >>= fun s ->
      match s with
      | None ->
          IO.return None
      | Some s ->
          IO.return @@ Some (Int64.to_int @@ EndianString.BigEndian.get_int64 s 0)

    let write_frame_to_buf ~mode buf fr =
      let open Frame in
      let scratch = Bytes.create 8 in
      let content = Bytes.unsafe_of_string fr.content in
      let len = Bytes.length content in
      let opcode = Opcode.to_enum fr.opcode in
      let payload_len = match len with
        | n when n < 126      -> len
        | n when n < 1 lsl 16 -> 126
        | _                   -> 127
      in
      let hdr = Bits.set_bit 0 15 (fr.final) in (* We do not support extensions for now *)
      let hdr = hdr lor (opcode lsl 8) in
      let hdr = Bits.set_bit hdr 7 (is_client mode) in
      let hdr = hdr lor payload_len in (* Payload len is guaranteed to fit in 7 bits *)
      EndianBytes.BigEndian.set_int16 scratch 0 hdr;
      Buffer.add_subbytes buf scratch 0 2;
      begin match len with
       | n when n < 126 -> ()
       | n when n < (1 lsl 16) ->
         EndianBytes.BigEndian.set_int16 scratch 0 n;
         Buffer.add_subbytes buf scratch 0 2
       | n ->
         EndianBytes.BigEndian.set_int64 scratch 0 Int64.(of_int n);
         Buffer.add_subbytes buf scratch 0 8;
      end;
      begin match mode with
      | Server -> ()
      | Client random_string ->
        let mask = random_string 4 in
        Buffer.add_string buf mask;
        if len > 0 then Bits.xor mask content;
      end;
      Buffer.add_bytes buf content

    let close_with_code mode buf oc code =
      Buffer.clear buf;
      write_frame_to_buf ~mode buf @@ Frame.close code;
      IO.write oc @@ Buffer.contents buf

    let make_read_frame ?(buf=Buffer.create 128) ~mode ic oc = fun () ->
      Buffer.clear buf;
      read_exactly ic 2 buf >>= fun hdr ->
      match hdr with
      | None -> raise End_of_file
      | Some hdr ->
        let hdr_part1 = EndianString.BigEndian.get_int8 hdr 0 in
        let hdr_part2 = EndianString.BigEndian.get_int8 hdr 1 in
        let final = Bits.is_bit_set 7 hdr_part1 in
        let extension = Bits.int_value 4 3 hdr_part1 in
        let opcode = Bits.int_value 0 4 hdr_part1 in
        let frame_masked = Bits.is_bit_set 7 hdr_part2 in
        let length = Bits.int_value 0 7 hdr_part2 in
        let opcode = Frame.Opcode.of_enum opcode in
        Buffer.clear buf;
        (match length with
        | i when i < 126 -> IO.return i
        | 126 -> (read_uint16 ic buf >>= function Some i -> IO.return i | None -> IO.return @@ -1)
        | 127 -> (read_int64 ic buf >>= function Some i -> IO.return i | None -> IO.return @@ -1)
        | _ -> IO.return @@ -1
        ) >>= fun payload_len ->
        if payload_len = -1 then
          raise (Protocol_error ("payload len = " ^ string_of_int length))
        else if extension <> 0 then
          close_with_code mode buf oc 1002 >>= fun () ->
          raise (Protocol_error "unsupported extension")
        else if Frame.Opcode.is_ctrl opcode && payload_len > 125 then
          close_with_code mode buf oc 1002 >>= fun () ->
          raise (Protocol_error "control frame too big")
        else
        (if frame_masked then
           (Buffer.clear buf;
            read_exactly ic 4 buf >>= function
            | None -> raise (Protocol_error "could not read mask");
            | Some mask -> IO.return mask)
         else IO.return String.empty) >>= fun mask ->
        if payload_len = 0 then
          IO.return @@ Frame.create ~opcode ~extension ~final ()
        else
        (Buffer.clear buf;
         read_exactly ic payload_len buf) >>= fun payload ->
        match payload with
        | None -> raise (Protocol_error "could not read payload")
        | Some payload ->
          let payload = Bytes.unsafe_of_string payload in
          if frame_masked then Bits.xor mask payload;
          let frame = Frame.of_bytes ~opcode ~extension ~final payload in
          IO.return frame

    type t = {
      buffer: Buffer.t;
      ic: IO.ic;
      oc: IO.oc;
      req : Cohttp.Request.t;
      read_frame: unit -> Frame.t IO.t;
    }

    let create
        ?read_buf
        ?(write_buf=Buffer.create 128)
        ~mode
        req
        ic oc =
      let read_frame = make_read_frame ?buf:read_buf ~mode ic oc in
      {
        buffer = write_buf;
        ic;
        oc;
        req;
        read_frame;
      }

    let req t = t.req

    let send { buffer; oc; _ } frame =
      Buffer.clear buffer;
      write_frame_to_buf ~mode:Server buffer frame;
      IO.write oc @@ Buffer.contents buffer

    let send_multiple { buffer; oc; _ } frames =
      Buffer.clear buffer;
      List.iter (write_frame_to_buf ~mode:Server buffer) frames;
      IO.write oc @@ Buffer.contents buffer

    let recv t =
      t.read_frame () >>= fun fr ->
      match fr.Frame.opcode with
      | Frame.Opcode.Ping ->
        send t @@ Frame.create ~opcode:Frame.Opcode.Pong () >>= fun () ->
        IO.return fr
      | Frame.Opcode.Close ->
        (* Immediately echo and pass this last message to the user *)
        (if String.length fr.Frame.content >= 2 then
           send t @@ Frame.create
             ~opcode:Frame.Opcode.Close
             ~content:(String.(sub ~start:0 ~stop:2 fr.Frame.content |> Sub.to_string)) ()
         else
           send t @@ Frame.close 1000
        ) >>= fun () ->
        IO.return fr
      | _ ->
        IO.return fr

    let upgrade_connection ?read_buf ?write_buf request handle_conn =
      let headers = Cohttp.Request.headers request in
      let key = match Cohttp.Header.get headers "sec-websocket-key" with
      | None -> failwith "upgrade_connection: missing header \
                              `sec-websocket-key`"
      | Some key -> key
      in
      let subprotocol = match Cohttp.Header.get headers "sec-websocket-protocol" with
      | None -> []
      | Some p -> ["Sec-WebSocket-Protocol", p]
      in
      let hash = b64_encoded_sha1sum (key ^ websocket_uuid) in
      let response_headers =
        Cohttp.Header.of_list
          (("Upgrade", "websocket") ::
          ("Connection", "Upgrade") ::
          ("Sec-WebSocket-Accept", hash) ::
          subprotocol)
      in
      let rsp =
          Cohttp.Response.make
            ~status:`Switching_protocols
            ~encoding:Cohttp.Transfer.Unknown
            ~headers:response_headers
            ~flush:true
            ()
      in
      let io_handler ic oc =
        let client = create ?read_buf ?write_buf ~mode:Server request ic oc in
        handle_conn client
      in
      `Expert (rsp, io_handler)
  end
end
