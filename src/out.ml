(*******************************************************************)
(*    This is part of WhyMyMon, and it is distributed under the    *)
(*     terms of the GNU Lesser General Public License version 3    *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2023:                                                *)
(*  Leonardo Lima (UCPH)                                           *)
(*  Oskar Eliassen (UCPH)                                          *)
(*  Niels Dylmer (UCPH)                                            *)
(*******************************************************************)

open Base
open Stdio
open Etc
open Checker_interface

module Plain = struct

  type t =
    | Explanation of (timestamp * timepoint) * Assignment.t * Expl.t
    | ExplanationCheck of (timestamp * timepoint) * Assignment.t * Expl.t * bool
    | ExplanationLatex of (timestamp * timepoint) * Assignment.t * Expl.t * Formula.t
    | ExplanationLight of (timestamp * timepoint) * Assignment.t * Expl.t
    | ExplanationCheckDebug of (timestamp * timepoint) * Assignment.t * Expl.t * bool * Checker_proof.t *
                                 Checker_trace.t

  let print = function
    | Explanation ((ts, tp),v, e) ->
       Stdio.printf "%d:%d\nExplanation: \n\n%s\n\n" ts tp (Expl.to_string e);
       Stdio.printf "\n%s\n" (Assignment.to_string v);
    | ExplanationCheck ((ts, tp),v, e, b) ->
       Stdio.printf "%d:%d\nExplanation: \n\n%s\n" ts tp (Expl.to_string e);
       Stdio.printf "\n%s\n" (Assignment.to_string v);
       Stdio.printf "\nChecker output: %B\n\n" b;
    | ExplanationLatex ((ts, tp), v,e, f) ->
       Stdio.printf "%d:%d\nExplanation: \n\n%s\n\n" ts tp (Expl.to_latex f e);
       Stdio.printf "\n%s\n" (Assignment.to_string v);
    | ExplanationLight ((ts, tp),v, e) ->
       Stdio.printf "%d:%d\nExplanation: \n\n%s\n\n" ts tp (Expl.to_light_string e);
       Stdio.printf "\n%s\n" (Assignment.to_string v);
    | ExplanationCheckDebug ((ts, tp), v, e, b, c_e, c_t) ->
       Stdio.printf "%d:%d\nExplanation: \n\n%s\n" ts tp (Expl.to_string e);
       Stdio.printf "\n%s\n" (Assignment.to_string v);
       Stdio.printf "\nChecker output: %B\n\n" b;
       Stdio.printf "\n[debug] Checker explanation:\n%s\n\n" (Checker_interface.Checker_proof.to_string "" c_e);
       Stdio.printf "\n[debug] Checker trace:\n%s" (Checker_interface.Checker_trace.to_string c_t);

end

(* TODO: Refactor this module (why are some of these functions here?) *)
module Json = struct

  let error err =
    Printf.sprintf "ERROR: %s" (Error.to_string_hum err)

  let table_columns f =
    let sig_preds_columns = List.rev (Set.fold (Formula.pred_names f) ~init:[] ~f:(fun acc r ->
                                          let r_props = Hashtbl.find_exn Pred.Sig.table r in
                                          let var_names = fst (List.unzip r_props.ntconsts) in
                                          (Printf.sprintf "%s(%s)" r
                                             (Etc.string_list_to_string ~sep:", " var_names)) :: acc)) in
    let subfs_columns = List.map (Formula.subfs_dfs f) ~f:Formula.op_to_string in
    let subfs_scope = List.map (Formula.subfs_scope f 0) ~f:(fun (i, (js, ks)) ->
                          Printf.sprintf "{\"col\": %d, \"leftCols\": %s, \"rightCols\": %s}"
                            i (Etc.int_list_to_json js) (Etc.int_list_to_json ks)) in
    let subfs = List.map (Formula.subfs_dfs f) ~f:(Formula.to_string true) in
    Printf.sprintf "{\"formula\": \"%s\", \"predsColumns\": %s, \"subfsColumns\": %s, \"subfsScopes\": [%s], \"subformulas\": %s}"
      (Formula.to_string false f)
      (Etc.string_list_to_json sig_preds_columns) (Etc.string_list_to_json subfs_columns)
      (Etc.string_list_to_string ~sep:", " subfs_scope) (Etc.string_list_to_json subfs)

  let db ts tp row db f =
    Printf.sprintf "%s{" (String.make 4 ' ') ^
      Printf.sprintf "%s\"ts\": %d," (String.make 8 ' ') ts ^
        Printf.sprintf "%s\"tp\": %d," (String.make 8 ' ') tp ^
          Printf.sprintf "%s\"row\": %d," (String.make 8 ' ') row ^
            Printf.sprintf "%s" (Vis.Dbs.to_json tp db f) ^
              Printf.sprintf "%s}" (String.make 4 ' ')

  let expl_row ts tp_offset f_e_opt =
    Printf.sprintf "%s{" (String.make 4 ' ') ^
      Printf.sprintf "%s\"ts\": %d," (String.make 8 ' ') ts ^
        Printf.sprintf "%s\"expl\": {" (String.make 8 ' ') ^
          (match f_e_opt with
           | None -> ""
           | Some (f, e) -> Printf.sprintf "%s" (Vis.Expl.to_json f (Expl.sort_parts e) tp_offset)) ^
            Printf.sprintf "}%s}" (String.make 4 ' ')

  let aggregate tp dbs expl_rows =
    Printf.sprintf "{" ^
      Printf.sprintf "%s\"tp\": %d," (String.make 4 ' ') tp ^
        Printf.sprintf "%s\"db_objs\": [" (String.make 4 ' ') ^
          String.concat ~sep:"," dbs ^
            Printf.sprintf "]," ^
              Printf.sprintf "%s\"expl_objs\": [" (String.make 4 ' ') ^
                String.concat ~sep:"," expl_rows ^
                  Printf.sprintf "]}"
end
