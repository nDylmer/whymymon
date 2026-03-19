(*******************************************************************)
(*    This is part of WhyMyMon, and it is distributed under the    *)
(*     terms of the GNU Lesser General Public License version 3    *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2024:                                                *)
(*  Leonardo Lima (UCPH)                                           *)
(*******************************************************************)

open Etc

val to_tpts_assignments: Argument.Monitor.t -> string list -> Dom.tt list -> string -> (timepoint * timestamp option * Assignment.t) list

val is_verdict: Argument.Monitor.t -> string -> bool

val parse_prog_tp: Argument.Monitor.t -> string -> int

val write_line: Argument.Monitor.t ->  timepoint * timestamp option * Db.t -> string

val args: Argument.Monitor.t -> mon_path:string -> ?sig_path:string -> f_path:string -> string list
