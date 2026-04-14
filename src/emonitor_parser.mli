(*******************************************************************)
(*    This is part of WhyMyMon, and it is distributed under the    *)
(*     terms of the GNU Lesser General Public License version 3    *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2024:                                                *)
(*  Leonardo Lima (UCPH)                                           *)
(*******************************************************************)

open Etc

module Monpoly : sig

  val parse: string -> timepoint * timestamp option * string list list

end
module Timelymon : sig 
  val parse: string -> timepoint * timestamp option * string list list
end
module DejaVu : sig 
  val parse: string -> timepoint * timestamp option * string list list
end