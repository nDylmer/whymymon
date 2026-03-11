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

(* TimelyMon expects explicit tp and ts per emitted fact line. *)

(* initialize counter for timepoint*)
let tp_ref = ref (-1)

(* resets counter*)
let reset () =
  tp_ref := -1

(* increments to next timepoint*)
let next_tp () =
  tp_ref := !tp_ref + 1;
  !tp_ref

(* converts internal valye to text, timelymon input is text*)
let atom_of_dom = function
  | Dom.Int i -> Int.to_string i
  | Dom.Str s -> Printf.sprintf "'%s'" s
  | Dom.Float f -> Printf.sprintf "'%s'" (Float.to_string f)

(* converts one event into one timelymon line*)
let event_line tp ts_opt ((pred, args) : Db.Event.t) =
  let formatted_args = 
    List.mapi args ~f:(fun i d -> Printf.sprintf "x%d=%s" i (atom_of_dom d)) in
  let csv_args = String.concat  ~sep:", " formatted_args in
  (match ts_opt with
  | Some ts -> 
              if String.is_empty csv_args then
                Printf.sprintf "%s, tp=%d, ts=%d\n" pred tp ts
              else
                Printf.sprintf "%s, tp=%d, ts=%d, %s\n" pred tp ts csv_args
  | None -> 
              if String.is_empty csv_args then
                Printf.sprintf "%s, tp=%d\n" pred tp
              else
                Printf.sprintf "%s, tp=%d, %s\n" pred tp csv_args)

(* converts all events in one database snapshot to lines, uses same tp and ts for these events*)
let encode_db tp ts db =
  let events = Set.to_list db in
  let lines = List.map events ~f:(event_line tp ts) in
  String.concat ~sep:"" lines

let watermark_line w =
  Printf.sprintf ">WATERMARK %d<\n" w