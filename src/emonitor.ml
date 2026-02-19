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

(* TODO: Rewrite this using functors/first-class modules to distinguish monitors (or maybe not) *)
let to_tpts_assignments (mon: Argument.Monitor.t) vars vars_tt line =
  match mon with
  | MonPoly
    | TimelyMon
      | VeriMon -> let (tp, ts, sss) = Emonitor_parser.Monpoly.parse line in
                 if List.is_empty sss then (tp, ts, [Assignment.init ()])
                 else
                   (tp, ts, List.map sss ~f:(fun ss ->
                                List.fold2_exn (List.zip_exn vars vars_tt) ss ~init:(Assignment.init ())
                                  ~f:(fun v (x, x_tt) s -> let d = Dom.string_to_t s x_tt in
                                                           Assignment.add v x d)))
  | DejaVu -> failwith "missing"
  


let is_verdict (mon: Argument.Monitor.t) line =
  match mon with
  | MonPoly
    | TimelyMon
      | VeriMon -> String.equal (String.prefix line 1) "@"
  | DejaVu -> failwith "missing"

let parse_prog_tp (mon: Argument.Monitor.t) line =
  match mon with
  | MonPoly
    | VeriMon -> Int.of_string (List.last_exn (String.split line ~on:' '))
  | DejaVu -> failwith "missing"
  | TimelyMon -> Int.min_value

let write_line (mon: Argument.Monitor.t) (ts, db) =
  match mon with
  | MonPoly
    | VeriMon -> "@" ^ (Int.to_string ts) ^ " " ^ Db.to_monpoly db
  | DejaVu -> failwith "missing"
  | TimelyMon -> Timelylog.encode_next_tp ~ts db

let args (mon: Argument.Monitor.t) ~mon_path ?sig_path ~f_path =
  match mon with
  | MonPoly -> [mon_path; "-sig"; Option.value_exn sig_path; "-formula";
                f_path; "-nonewlastts"; "-nofilteremptytp"; "-nofilterrel"];
  | VeriMon -> [mon_path; "-sig"; Option.value_exn sig_path; "-formula";
                f_path; "-nonewlastts"; "-nofilteremptytp"; "-nofilterrel"; "-verified"];
  | DejaVu -> failwith "missing"
  | TimelyMon -> 
    Timelylog.reset();
    [mon_path;"-s";Option.value_exn sig_path; f_path;]
