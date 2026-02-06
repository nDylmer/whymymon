(*******************************************************************)
(*    This is part of WhyMyMon, and it is distributed under the    *)
(*     terms of the GNU Lesser General Public License version 3    *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2024:                                                *)
(*  Leonardo Lima (UCPH)                                           *)
(*******************************************************************)

module Preference : sig

  type t = Satisfaction | Violation

  val of_string: string -> t

  val to_string: t -> string

end

module Monitor : sig

  type t = MonPoly | VeriMon | DejaVu | TimelyMon

  val of_string: string -> t

  val to_string: t -> string

  val exec_path: t -> string

  val extra_args: Preference.t -> t -> string list

end

module Mode : sig

  type t = Unverified | Verified | LaTeX | Debug | DebugVis

  val of_string: string -> t

  val to_string: t -> string

end

module Interface : sig

  type t = GUI | CLI

end
