(*******************************************************************)
(*     This is part of WhyMon, and it is distributed under the     *)
(*     terms of the GNU Lesser General Public License version 3    *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2023:                                                *)
(*  Leonardo Lima (UCPH)                                           *)
(*  Oskar Eliassen (UCPH)                                          *)
(*  Niels Dylmer (UCPH)                                            *)
(*******************************************************************)

open Base
open Etc
open Checker_interface

module Plain : sig

  type t =
    | Explanation of (timestamp * timepoint) * Assignment.t * Expl.t
    | ExplanationCheck of (timestamp * timepoint) * Assignment.t * Expl.t * bool
    | ExplanationLatex of (timestamp * timepoint) * Assignment.t * Expl.t * Formula.t
    | ExplanationLight of (timestamp * timepoint) * Assignment.t * Expl.t
    | ExplanationCheckDebug of (timestamp * timepoint) * Assignment.t * Expl.t * bool * Checker_proof.t *
                                 Checker_trace.t
  val print: t -> unit

end

module Json : sig

  val table_columns: Formula.t -> string

  val db: timestamp -> timepoint -> int -> Db.t -> Formula.t -> string

  val expl_row : timestamp -> int -> (Formula.t * Expl.t) option -> string

  val aggregate: timepoint ->  string list -> string list -> string

end
