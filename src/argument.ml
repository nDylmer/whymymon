(*******************************************************************)
(*    This is part of WhyMyMon, and it is distributed under the    *)
(*     terms of the GNU Lesser General Public License version 3    *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2024:                                                *)
(*  Leonardo Lima (UCPH)                                           *)
(*  Oskar Eliassen (UCPH)                                          *)
(*  Niels Dylmer (UCPH)                                            *)
(*******************************************************************)

open Base

module Preference = struct

  type t = Satisfaction | Violation

  let of_string = function
    | "sat" | "satisfaction" -> Satisfaction
    | "vio" | "violation" -> Violation
    | _ -> Format.eprintf "your preference should be either: satisfaction or violation\n%!";
           raise (Invalid_argument "undefined preference")

  let to_string = function
    | Satisfaction -> "Satisfaction"
    | Violation -> "Violation"

end

module Monitor = struct

  type t = MonPoly | VeriMon | DejaVu | TimelyMon

  let of_string = function
    | "monpoly" | "MonPoly" | "Monpoly" -> MonPoly
    | "verimon" | "VeriMon" | "Verimon" -> VeriMon
    | "dejavu" | "DejaVu" | "Dejavu" -> DejaVu
    | "timelymon" | "TimelyMon" | "Timelymon" -> TimelyMon
    | _ -> Format.eprintf "monitors supported: monpoly, verimon, timelymon or dejavu\n%!";
           raise (Invalid_argument "undefined monitor")

  let to_string = function
    | MonPoly -> "MonPoly"
    | VeriMon -> "VeriMon"
    | DejaVu -> "DejaVu"
    | TimelyMon -> "TimelyMon"

  let exec_path = function
    | MonPoly
      | VeriMon -> "_build/install/default/bin/monpoly"
    | DejaVu -> failwith "not yet"
    | TimelyMon -> "third-party/timelymon/target/release/timelymon"

  let extra_args (pref: Preference.t) = function
    | MonPoly
      | VeriMon -> (match pref with
                    | Satisfaction -> []
                    | Violation -> ["-negate"])

    | TimelyMon -> (match pref with
                    | Satisfaction -> ["-m";"1"]
                    | Violation -> failwith "not yet")
              
      
    | DejaVu -> failwith "not yet"

end

module Mode = struct

  type t = Unverified | Verified | LaTeX | Debug | DebugVis

  let of_string = function
    | "unverified" -> Unverified
    | "verified" -> Verified
    | "latex" -> LaTeX
    | "debug" -> Debug
    | "debugvis" -> DebugVis
    | _ -> Format.eprintf "modes: unverified or verified\n%!";
           raise (Invalid_argument "undefined mode")

  let to_string = function
    | Unverified -> "Unverified"
    | Verified -> "Verified"
    | LaTeX -> "LaTeX"
    | Debug -> "Debug"
    | DebugVis -> "DebugVis"

end

module Interface = struct

  type t = GUI | CLI

end
