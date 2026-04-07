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
open Etc
type out_item =
  | Events of timepoint * timestamp option * Db.t
  | Watermark of int




(* TODO: Rewrite this using functors/first-class modules to distinguish monitors (or maybe not) *)
let to_tpts_assignments (mon: Argument.Monitor.t) vars vars_tt line =
  match mon with
  | DejaVu -> failwith "not yet"
  | MonPoly
  | VeriMon -> let (tp, ts, sss) = Emonitor_parser.Monpoly.parse line in
                 if List.is_empty sss then [(tp, ts, Assignment.init ())]
                 else

                   List.map sss ~f:(fun ss ->
                                List.fold2_exn (List.zip_exn vars vars_tt) ss ~init: (tp, ts,Assignment.init ())
                                  ~f:(fun (tp,ts,v) (x, x_tt) s -> let d = Dom.string_to_t s x_tt in
                                                          ( tp, ts , Assignment.add v x d)))
  | TimelyMon -> 
                let (tp, tp_up_opt, sss) = Emonitor_parser.Timelymon.parse line in
                match tp_up_opt with
                | None -> 
                 if List.is_empty sss then [(tp, None, Assignment.init ())]
                 else

                   List.map sss ~f:(fun ss ->
                                List.fold2_exn (List.zip_exn vars vars_tt) ss ~init: (tp, None ,Assignment.init ())
                                  ~f:(fun (tp,ts,v) (x, x_tt) s -> let d = Dom.string_to_t s x_tt in
                                                          ( tp, None , Assignment.add v x d)))
                                                          
                | Some tp_up ->
                  let tps = List.range tp (tp_up + 1) in
                  if List.is_empty sss then
                  List.map tps ~f:(fun tp -> (tp, None, Assignment.init ()))
                   else
                  List.concat_map sss ~f:(fun ss ->
                    let v =
                      List.fold2_exn (List.zip_exn vars vars_tt) ss
                        ~init:(Assignment.init ())
                        ~f:(fun acc (x, x_tt) s ->
                          let d = Dom.string_to_t s x_tt in
                          Assignment.add acc x d)
                    in
                    List.map tps ~f:(fun tp -> (tp, None, v)))
  


let is_verdict (mon: Argument.Monitor.t) line =
  match mon with
  | MonPoly
  | VeriMon -> String.equal (String.prefix line 1) "@"
  | TimelyMon -> String.equal (String.prefix line 1) "("
  | DejaVu -> failwith "missing"

let parse_prog_tp (mon: Argument.Monitor.t) line =
  match mon with
  | MonPoly
    | VeriMon -> Int.of_string (List.last_exn (String.split line ~on:' '))
  | DejaVu -> failwith "missing"
  | TimelyMon -> Int.min_value

let write_line (mon: Argument.Monitor.t) (tp, ts_opt, db) =
  match mon with
  | MonPoly
    | VeriMon -> 
              (match ts_opt with
              | Some ts -> "@" ^ Int.to_string ts ^ " " ^ Db.to_monpoly db
              | None -> failwith "MonPoly/VeriMon require timestamp")
  | DejaVu -> failwith "missing"
  | TimelyMon -> 
    let tp = if !Etc.log_is_csv then tp else Timelylog.next_tp() in
    Timelylog.encode_db tp ts_opt db
            

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


let write_item (mon: Argument.Monitor.t) = function
  | Events (tp, ts, db) -> write_line mon (tp, ts, db)
  | Watermark w ->
    (match mon with
     | TimelyMon -> Timelylog.watermark_line w
     | MonPoly | VeriMon | DejaVu -> "")
