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
    opcode: Opcode.t ;
    extension: int ;
    final: bool ;
    content: string ;
  }

  val create :
    ?opcode:Opcode.t ->
    ?extension:int ->
    ?final:bool ->
    ?content:string ->
    unit -> t

  val close : int -> t
  val show : t -> string
end

module Connection : sig
  type mode =
    | Client of (int -> string)
    | Server

  module type S = sig
    module IO : Cohttp.S.IO

    type t

    val create :
      ?read_buf:Buffer.t -> ?write_buf:Buffer.t ->
      mode:mode -> Cohttp.Request.t -> IO.ic -> IO.oc -> t

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

  module Make (IO : Cohttp.S.IO) : S with module IO = IO
end
