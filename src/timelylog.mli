(*******************************************************************)
(*     This is part of WhyMon, and it is distributed under the     *)
(*     terms of the GNU Lesser General Public License version 3    *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2023:                                                *)
(*  Oskar Eliassen (UCPH)                                          *)
(*  Niels Dylmer (UCPH)                                            *)
(*******************************************************************)
open Base

val reset : unit -> unit
val next_tp : unit -> int

val atom_of_dom : Dom.t -> string

val event_line : int -> int option -> Db.Event.t -> string
val encode_db : int -> int option -> Db.t -> string

val watermark_line : int -> string
