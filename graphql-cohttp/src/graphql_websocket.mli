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

exception Protocol_error of string

(** Opening handshake of a client (RFC 6455, section 4.2.1). *)
module Handshake : sig
  val is_upgrade : Cohttp.Header.t -> bool
  (** [is_upgrade headers] is [true] iff [headers] ask for a websocket upgrade:
      the [Connection] header includes the token [upgrade] and the [Upgrade]
      header the token [websocket], ignoring case. *)

  val version : string
  (** [version] is ["13"], the only version of the protocol supported. *)

  type error =
    | Unsupported_version
        (** The [Sec-WebSocket-Version] header is missing, repeated, or not
            {!version}. *)
    | Invalid_key
        (** The [Sec-WebSocket-Key] header is missing, repeated, or not the
            base64 encoding of 16 bytes. *)

  val check : Cohttp.Header.t -> (unit, error) result
  (** [check headers] checks the [Sec-WebSocket-Version] and
      [Sec-WebSocket-Key] headers of an upgrade request (see {!is_upgrade}),
      in this order. *)
end

module Frame : sig
  module Opcode : sig
    type t =
      | Continuation
      | Text
      | Binary
      | Close
      | Ping
      | Pong
      | Ctrl of int
      | Nonctrl of int

    val to_string : t -> string

    val pp : Format.formatter -> t -> unit
  end

  type t = {
    opcode : Opcode.t;
    extension : int;
    final : bool;
    content : string;
  }

  val create :
    ?opcode:Opcode.t ->
    ?extension:int ->
    ?final:bool ->
    ?content:string ->
    unit ->
    t

  val close : int -> t

  val show : t -> string
end

module Connection : sig
  type mode = Client of (int -> string) | Server

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
      ?read_buf:Buffer.t ->
      ?write_buf:Buffer.t ->
      ?protocols:string list ->
      Cohttp.Request.t ->
      (t -> unit IO.t) ->
      [> `Expert of Cohttp.Response.t * (IO.ic -> IO.oc -> unit IO.t) ]
    (** [upgrade_connection ~protocols req handle_conn] is the response that
        accepts the websocket upgrade requested by [req], and then runs
        [handle_conn] on the connection. The response selects the first
        subprotocol offered by the client that is in [protocols] (default
        [[]]), and selects none if there is no such subprotocol. It does not
        validate [req]: use {!Handshake.check} first.

        @raise Failure if [req] has no [Sec-WebSocket-Key] header. *)
  end

  module Make (IO : Cohttp.S.IO) : S with module IO = IO
end
