(*******************************************************************)
(*     This is part of WhyMon, and it is distributed under the     *)
(*     terms of the GNU Lesser General Public License version 3    *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2023:                                                *)
(*  Dmitriy Traytel (UCPH)                                         *)
(*  Leonardo Lima (UCPH)                                           *)
(*******************************************************************)

open Base
open Etc

module Parsebuf : sig

  type t = { lexbuf: Lexing.lexbuf
           ; mutable token: Other_lexer.token
           ; mutable pred_sig: Pred.Sig.t option
           ; mutable ts: int
           ; mutable tp: int
           ; mutable db: Db.t }

end

module Sig : sig

  val parse_from_channel: string -> unit

  val parse_from_string: string -> unit

end

module CSV : sig

  type cursor = Processed of Parsebuf.t
              | Skipped   of Parsebuf.t * string
              | Watermark of int
              | Finished
  val predicate: string -> string
  val parse_key_value: string -> (string * string) option
  val parse_event: string -> (int * int * Db.t) option
  val parse_from_channel: Stdio.In_channel.t -> Parsebuf.t option -> cursor
end


module Trace : sig

  type cursor = Processed of Parsebuf.t
              | Skipped   of Parsebuf.t * string
              | Watermark of int
              | Finished

  val parse_from_channel: Stdio.In_channel.t -> Parsebuf.t option -> cursor

  val parse_from_string: string -> cursor

  val parse_line: string -> (timestamp * Db.t) option

end


