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

open Core
open Etc
open Expl
open Pred
open Eio.Std
open Argument
module Quantifier = struct

  type t = Existential | Universal

end

let writer_done = ref false
type range =
  | Lit of timepoint * timestamp
  | Int of timepoint * timepoint * timestamp * timestamp
  | IInt of timepoint * timestamp

type obs_seq =
  range list

type prefix = {
  mutable obs_seq : obs_seq;
  dbs : (timepoint, Db.t) Hashtbl.t;
}
type upper_bound = 
  | Finite of int
  | Infinity

let db_at prefix tp =
  match Hashtbl.find prefix.dbs tp with
  | Some db -> db
  | None -> Db.create []

let add_db_union prefix tp db =
  let old_db =
    match Hashtbl.find prefix.dbs tp with
    | Some old_db -> old_db
    | None -> Db.create []
  in
  Hashtbl.set prefix.dbs ~key:tp ~data:(Set.union old_db db)


let compare_tp tp range =
  match range with
  | Lit (a,_) -> 
        if tp < a then -1 
        else if tp > a then 1
        else 0
  | Int (a,b,_,_) ->
        if tp < a then -1 
        else if tp > b then 1
        else 0
  | IInt (a,_) ->
        if tp < a then -1 
        else 0

let rec find_range_by_tp obs_seq tp =
  match obs_seq with
  | [] -> None
  | range :: rest ->
      match compare_tp tp range with
      | 0 -> Some range
      | -1 -> None
      | 1 -> find_range_by_tp rest tp
      | _ -> failwith "impossible compare result"


let split_range_at range tp ts =
  match range with
  | Lit (a, _) ->
        [range]

  | Int (a, b, ts_l, ts_u) ->
      let left =
        if tp > a then [Int (a, tp - 1, ts_l, ts)] else []
      in
      let middle = [Lit (tp, ts)] in
      let right =
        if tp < b then [Int (tp + 1, b, ts, ts_u)] else []
      in
      left @ middle @ right

  | IInt (a, ts_l) ->
      let left =
        if tp > a then [Int (a, tp - 1, ts_l, ts)] else []
      in
      let middle = [Lit (tp, ts)] in
      let right = [IInt (tp + 1, ts)] in
      left @ middle @ right

let insert_ts prefix tp ts =
  let rec aux acc ranges =
    match ranges with
    | [] ->
        failwith "insert_ts: no range contains tp"

    | range :: rest ->
        match compare_tp tp range with
        | 0 ->
            let replacement = split_range_at range tp ts in
            List.rev_append acc (replacement @ rest)
        | -1 ->
            failwith "insert_ts: tp before current range"
        | 1 ->
            aux (range :: acc) rest
        | _ ->
            failwith "impossible compare result"
  in
  prefix.obs_seq <- aux [] prefix.obs_seq


let insert_prefix prefix tp ts_opt db =
  add_db_union prefix tp db;
  match ts_opt with
  | Some ts -> insert_ts prefix tp ts
  | None -> ()


let timestamp_interval_of_range range =
  match range with
  | Lit (_, ts) -> (ts, Finite ts)
  | Int (_, _, ts_lo, ts_hi) -> (ts_lo, Finite ts_hi)
  | IInt (_, ts_lo) -> (ts_lo, Infinity)

let timestamp_interval prefix tp =
  match find_range_by_tp prefix.obs_seq tp with
  | Some range -> timestamp_interval_of_range range
  | None -> (0, Infinity)

let leq_upper x up =
  match up with
  | Finite u -> x <= u
  | Infinity -> true

let interval_may_overlap l r (ts_l, ts_u) =
  leq_upper ts_l r && leq_upper l ts_u

let past_candidate_tps prefix tp l r =
  let keys = Hashtbl.keys prefix.dbs in
  let candidates =
    List.filter keys ~f:(fun tp' ->
      tp' <= tp &&
      interval_may_overlap l r (timestamp_interval prefix tp'))
  in
  Set.of_list (module Int) candidates

let future_candidate_tps prefix tp l r =
  let keys = Hashtbl.keys prefix.dbs in
  let candidates =
    List.filter keys ~f:(fun tp' ->
      tp' >= tp &&
      interval_may_overlap l r (timestamp_interval prefix tp'))
  in
  Set.of_list (module Int) candidates


let prefix_max_tp prefix =
  let keys = Hashtbl.keys prefix.dbs in
  match List.max_elt keys ~compare:Int.compare with
  | Some tp -> tp
  | None -> -1

let prev_in_set s tp =
  Set.binary_search s ~compare:Int.compare `Last_strictly_less_than tp

let next_in_set s tp =
  Set.binary_search s ~compare:Int.compare `First_strictly_greater_than tp

let do_neg (p_opt: Proof.t option) (pol: Polarity.t) =
  match p_opt, pol with
  | Some (S sp), VIO -> Some (Proof.V (VNeg sp))
  | Some (V vp), SAT -> Some (S (SNeg vp))
  | _ -> None

let do_and (p1_opt: Proof.t option) (p2_opt: Proof.t option) (pol: Polarity.t) : Proof.t option =
  Proof.Size.minp_list
    (match p1_opt, p2_opt, pol with
     | Some (S sp1), Some (S sp2), SAT -> [Proof.S (SAnd (sp1, sp2))]
     | None, Some (V vp2), VIO
       | Some (S _), Some (V vp2), VIO -> [V (VAndR (vp2))]
     | Some (V vp1), None, VIO
       | Some (V vp1), Some (S _), VIO -> [V (VAndL (vp1))]
     | Some (V vp1), Some (V vp2), VIO -> [(V (VAndL (vp1))); (V (VAndR (vp2)))]
     | _ -> [])

let do_or (p1_opt: Proof.t option) (p2_opt: Proof.t option) (pol: Polarity.t) : Proof.t option =
  Proof.Size.minp_list
    (match p1_opt, p2_opt, pol with
     | Some (S sp1), Some (S sp2), SAT -> [(S (SOrL (sp1))); (S (SOrR(sp2)))]
     | Some (S sp1), None, SAT
       | Some (S sp1), Some (V _), SAT -> [S (SOrL (sp1))]
     | None, Some (S sp2), SAT
       | Some (V _), Some (S sp2), SAT -> [S (SOrR (sp2))]
     | Some (V vp1), Some (V vp2), VIO -> [V (VOr (vp1, vp2))]
     | _ -> [])

let do_imp (p1_opt: Proof.t option) (p2_opt: Proof.t option) (pol: Polarity.t) : Proof.t option =
  Proof.Size.minp_list
    (match p1_opt, p2_opt, pol with
     | Some (S _), Some (S sp2), SAT -> [S (SImpR sp2)]
     | Some (S sp1), Some (V vp2), VIO -> [V (VImp (sp1, vp2))]
     | Some (V vp1), Some (S sp2), SAT -> [S (SImpL vp1); S (SImpR sp2)]
     | Some (V vp1), None, SAT
       | Some (V vp1), Some (V _), SAT -> [S (SImpL vp1)]
     | _ -> [])

let do_iff (p1_opt: Proof.t option) (p2_opt: Proof.t option) (pol: Polarity.t) : Proof.t option =
  match p1_opt, p2_opt, pol with
  | Some (S sp1), Some (S sp2), SAT -> Some (S (SIffSS (sp1, sp2)))
  | Some (S sp1), Some (V vp2), VIO -> Some (V (VIffSV (sp1, vp2)))
  | Some (V vp1), Some (S sp2), VIO -> Some (V (VIffVS (vp1, sp2)))
  | Some (V vp1), Some (V vp2), SAT -> Some (S (SIffVV (vp1, vp2)))

let do_exists_leaf x tc = function
  | Some p -> (match p with
               | Proof.S sp -> Some (Proof.S (SExists (x, Dom.tt_default tc, sp)))
               | V vp -> Some (V (VExists (x, Part.trivial vp))))
  | None -> None

let do_exists_node x tc part =
  if Part.exists part Proof.opt_isS then
    (let sats = Part.filter part Proof.opt_isS in
     (Part.values (Part.map2_dedup Proof.opt_equal sats (fun (s, p) ->
                       match p with
                       | Some (S sp) -> (let witness = Setc.some_elt tc s in
                                         (Setc.Finite (Set.of_list (module Dom) [witness]),
                                          Some (Proof.S (Proof.SExists (x, Setc.some_elt tc s, sp)))))
                       | Some (V vp) -> raise (Invalid_argument "found V proof in S partition")
                       | None -> raise (Invalid_argument "found None in Some partition")))))
  else
    if Part.for_all part Proof.opt_isV then
    [Some (V (Proof.VExists (x, Part.map_dedup Proof.v_equal part Proof.opt_unV)))]
    else []
let do_forall_leaf x tc = function
  | Some p -> (match p with
               | Proof.S sp -> Some (Proof.S (SForall (x, Part.trivial sp)))
               | V vp -> Some (Proof.V (VForall (x, Dom.tt_default tc, vp))))
  | None -> None

let do_forall_node x tc part =
  if Part.for_all part Proof.opt_isS then
    [Some (Proof.S (SForall (x, Part.map_dedup Proof.s_equal part Proof.opt_unS)))]
  else
    (let vios = Part.filter part (fun p -> Proof.opt_isV p) in
     (Part.values (Part.map2_dedup Proof.opt_equal vios (fun (s, p) ->
                       match p with
                       | Some (V vp) -> let witness = Setc.some_elt tc s in
                                        (Setc.Finite (Set.of_list (module Dom) [witness]),
                                         Some (Proof.V (Proof.VForall (x, Setc.some_elt tc s, vp))))
                       | Some (S sp) -> raise (Invalid_argument "found S proof in V partition")
                       | None -> raise (Invalid_argument "found None in Some partition")))))

let maps_to_string maps =
  "> Map:" ^ String.concat (List.join (List.map maps ~f:(fun map ->
                                           List.map (Map.to_alist map) ~f:(fun (k, v) ->
                                               Printf.sprintf "%s -> %s\n" k (Dom.to_string v)))))

let do_prev i (p_opt: Proof.t option) (prev_l,prev_u) (cur_l,cur_u) (pol: Polarity.t) =
  let l = match Interval.right i with
    | None -> 0
    | Some b -> 
    (match cur_u with 
    | Infinity -> 0 
    | Finite cu -> cur_l - b) in
  let r = match cur_u with
    | Infinity -> Infinity
    | Finite cu -> Finite (cu - Interval.left i) in
    let is_candidate = interval_may_overlap l r (prev_l, prev_u) in
  Proof.Size.minp_list
    (match p_opt, pol with
     | Some (S sp), SAT -> if is_candidate then
                             [Proof.S (SPrev sp)]
                           else []
     | Some (V vp), VIO -> (if not is_candidate then
                           (
                            match r with
                            | Infinity -> [V (VPrev vp)]
                            | Finite u -> if prev_l > u then
                             [Proof.V (VPrevOutL ((Proof.v_at vp)+1))]
                           else
                                [Proof.V (VPrevOutR ((Proof.v_at vp)+1))])
                              else [V (VPrev vp)])
     | _ -> [])

let do_next i (p_opt: Proof.t option) (cur_l,cur_u) (next_l,next_u)  (pol: Polarity.t) =
   match cur_u with
  | Infinity -> Proof.Size.minp_list []
  | Finite cur_u ->
    let l = cur_l + Interval.left i in
    let r = match Interval.right i with
    | None -> Infinity
    | Some b -> Finite (cur_u + b) in
  let is_candidate = interval_may_overlap l r (next_l,next_u) in
  Proof.Size.minp_list
    (match p_opt, pol with
     | Some (S sp), SAT -> if is_candidate then
                             [S (SNext sp)]
                           else []
     | Some (V vp), VIO -> (if not is_candidate then
                           (
                            match r with
                            | Infinity -> [V (VNext vp)]
                            | Finite u -> if next_l > u then
                             [V (VNextOutL ((Proof.v_at vp)-1))]
                           else
                                [V (VNextOutR ((Proof.v_at vp)-1))])
                           else [V (VNext vp)])
     | _ -> [])

let rec match_terms trms ds map =
  match trms, ds with
  | [], [] -> Some(map)
  | Term.Const c :: trms, d :: ds -> if Dom.equal c d then match_terms trms ds map else None
  | Var x :: trms, d :: ds -> (match match_terms trms ds map with
                               | None -> None
                               | Some(map') -> (match Map.find map' x with
                                                | None -> let map'' = Map.add_exn map' ~key:x ~data:d in
                                                          Some(map'')
                                                | Some z -> (if Dom.equal d z then Some map' else None)))
  | _, _ -> None

let rec pdt_of tp r trms (vars: string list) maps = match vars with
  | [] -> if List.is_empty maps then Pdt.Leaf (Proof.V (VPred (tp, r, trms)))
          else Leaf (S (SPred (tp, r, trms)))
  | x :: vars ->
     let ds = List.fold maps ~init:[]
                ~f:(fun acc map -> match Map.find map x with
                                   | None -> acc
                                   | Some(d) -> d :: acc) in
     let find_maps d = List.fold maps ~init:[]
                         ~f:(fun acc map -> match Map.find map x with
                                            | None -> acc
                                            | Some(d') -> if Dom.equal d d' then
                                                            map :: acc
                                                          else acc) in
     let part = Part.tabulate_dedup (Pdt.equal Proof.equal) (Set.of_list (module Dom) ds)
                  (fun d -> pdt_of tp r trms vars (find_maps d)) (pdt_of tp r trms vars []) in
     Node (x, part)


module State = struct

  type t = { f: Formula.t
           ; pol: Polarity.t
           ; tp: timepoint
           ; expl: Expl.t }

end


let either_s_equal e e' = match e, e' with
  | First p, First p' -> Proof.opt_equal p p'
  | Second sps, Second sps' -> Etc.fdeque_for_all2 sps sps' ~f:Proof.s_equal
  | _ -> false

let either_v_equal e e' = match e, e' with
  | First p, First p' -> Proof.opt_equal p p'
  | Second vps, Second vps' -> Etc.fdeque_for_all2 vps vps' ~f:Proof.v_equal
  | _ -> false

let either_v_equal2 e e' = match e, e' with
  | First p, First p' -> Proof.opt_equal p p'
  | Second (vp, vps), Second (vp', vps') -> Proof.opt_v_equal vp vp' &&
                                              Etc.fdeque_for_all2 vps vps' ~f:Proof.v_equal
  | _ -> false

(* Note that the polarity pol considered is the one on the bottom level *)
let rec stop_either vars vars_map expl (pol: Polarity.t) =
  (* traceln "STOP_EITHER |vars| = %d; pol = %s" (List.length vars) (Polarity.to_string pol); *)
  match vars, expl, pol with
  | [], Pdt.Leaf (Either.First (Some (Proof.S _))), SAT -> true
  | [], Leaf (First (Some (V _))), VIO -> true
  | x :: xs, Node (y, part), _ when String.equal x y ->
     let (kind, pol) = Map.find_exn vars_map x in
     (match kind, pol with
      | Quantifier.Existential, Polarity.SAT
        | Universal, VIO -> Part.exists part (fun expl -> stop_either xs vars_map expl pol)
      | Existential, VIO
        | Universal, SAT -> Part.for_all part (fun expl -> stop_either xs vars_map expl pol)
      | _ -> raise (Failure "stop: issue with variable ordering"))
  | _ -> false

let rec stop vars vars_map expl (pol: Polarity.t) = match vars, expl, pol with
  | [], Pdt.Leaf (Some (Proof.S _)), SAT -> true
  | [], Leaf (Some (V _)), VIO -> true
  | x :: xs, Node (y, part), _ when String.equal x y ->
    let (kind, pol) = Map.find_exn vars_map x in
     (match kind, pol with
     | Quantifier.Existential, Polarity.SAT
       | Universal, VIO -> Part.exists part (fun expl -> stop xs vars_map expl pol)
     | Existential, VIO
       | Universal, SAT -> Part.for_all part (fun expl -> stop xs vars_map expl pol)
     | _ -> raise (Failure "stop: issue with variable ordering"))
  | _ -> false
  

let explain prefix v pol tp f =
  (* traceln "assignment: %s" (Assignment.to_string v); 
  traceln "tp = %d" tp; *)
  let rec eval vars vars_map tp (pol: Polarity.t) (f: Formula.t) = match f with
    | TT ->
       (match pol with
        | SAT -> Pdt.Leaf (Some (Expl.Proof.S (STT tp)))
        | VIO -> Pdt.Leaf None)
    | FF ->
       (match pol with
        | SAT -> Pdt.Leaf None
        | VIO -> Pdt.Leaf (Some (Expl.Proof.V (VFF tp))))
    | EqConst (x, d) ->
       let l1 = Pdt.Leaf (Some (Proof.S (SEqConst (tp, x, d)))) in
       let l2 = Pdt.Leaf (Some (Proof.V (VEqConst (tp, x, d)))) in
       (match pol, Map.find v x with
        | SAT, Some d' when Dom.equal d d' -> l1
        | VIO, Some d' when not (Dom.equal d d') -> l2
        | SAT, None ->
           Pdt.Node (x, Part.of_list [(Setc.Complement (Set.of_list (module Dom) [d]), l2);
                                      (Setc.Finite (Set.of_list (module Dom) [d]), l1)])
        | VIO, None ->
           Pdt.Node (x, Part.of_list [(Setc.Complement (Set.of_list (module Dom) [d]), l1);
                                      (Setc.Finite (Set.of_list (module Dom) [d]), l2)]))
    | Predicate (r, trms) ->
       (* Replace trms with values coming from variable assignment v *)
       let trms_subst = List.map trms ~f:(fun trm -> if Pred.Term.is_var trm then
                                                       (match Map.find v (Pred.Term.unvar trm) with
                                                        | None -> (* traceln "Var = %s" (Term.to_string trm); *)
                                                           trm
                                                        | Some d -> Const d)
                                                     else trm) in
       let db = Set.filter (db_at prefix tp) ~f:(fun evt -> String.equal r (fst(evt))) in
       let maps = Set.fold db ~init:[] ~f:(fun acc evt -> match_terms trms_subst (snd evt)
                                                            (Map.empty (module String)) :: acc) in
       (* traceln "|maps| = %d" (List.length maps); *)
       let maps' = List.map (List.filter maps ~f:(fun map_opt -> Option.is_some map_opt))
                     ~f:(fun map_opt -> Option.value_exn map_opt) in
       (* traceln "maps = %s" (maps_to_string maps'); *)
       let fvs = Set.of_list (module String) (Pred.Term.fv_list trms_subst) in
       (* traceln "|fvs| = %d" (Set.length fvs); *)
       (* traceln "|vars| = %d" (List.length vars);*) 
       let vars = List.filter vars ~f:(fun x -> Set.mem fvs x) in
       (* if !Etc.debug then  traceln "|vars| = %d" (List.length vars); *)
       let expl = Pdt.somes_pol pol (pdt_of tp r trms vars maps') in
        (* traceln "PREDICATE %s; %s expl = %s" r (Polarity.to_string pol) (Expl.opt_to_string expl); *)
       expl
    | Neg f ->
       let expl = eval vars vars_map tp (Polarity.invert pol) f in
       let expl = Pdt.apply1_reduce Proof.opt_equal vars
                    (fun p_opt -> do_neg p_opt pol) expl in
       (* traceln "NEG %s expl = %s" (Polarity.to_string pol) (Expl.opt_to_string expl); *)
       expl
    | And (f1, f2) ->
       let expl1 = eval vars vars_map tp pol f1 in
       if Expl.opt_all_none expl1 then
         Pdt.Leaf None
       else
         (let expl2 = eval vars vars_map tp pol f2 in
          let expl = Pdt.apply2_reduce Proof.opt_equal vars
                       (fun p1_opt p2_opt -> (do_and p1_opt p2_opt pol)) expl1 expl2 in
          (* traceln "AND expl = %s" (Expl.opt_to_string expl); *)
          expl)
    | Or (f1, f2) ->
       let expl1 = eval vars vars_map tp pol f1 in
       let expl2 = eval vars vars_map tp pol f2 in
       Pdt.apply2_reduce Proof.opt_equal vars
         (fun p1_opt p2_opt -> (do_or p1_opt p2_opt pol)) expl1 expl2
    | Imp (f1, f2) ->
       let expl1 = eval vars vars_map tp (Polarity.invert pol) f1 in
       (match pol, Expl.opt_exists_satisfaction expl1 with
        | VIO, false -> Pdt.Leaf None
        | _ -> let expl2 = eval vars vars_map tp pol f2 in
               let expl = Pdt.apply2_reduce Proof.opt_equal vars
                            (fun p1_opt p2_opt -> (do_imp p1_opt p2_opt pol)) expl1 expl2 in
               (* traceln "IMP expl = %s" (Expl.opt_to_string expl); *)
               expl)
    | Iff (f1, f2) ->
       let (expl1, expl2) =
         (match pol with
          | SAT -> let s_e1 = eval vars vars_map tp SAT f1 in
                   if Expl.opt_all_none s_e1 then
                     (let v_e1 = eval vars vars_map tp VIO f1 in
                      let v_e2 = eval vars vars_map tp VIO f2 in
                      (v_e1, v_e2))
                   else
                     (let s_e2 = eval vars vars_map tp SAT f2 in
                      (s_e1, s_e2))
          | VIO -> let s_e1 = eval vars vars_map tp SAT f1 in
                   if Expl.opt_all_none s_e1 then
                     (let v_e1 = eval vars vars_map tp VIO f1 in
                      let s_e2 = eval vars vars_map tp SAT f2 in
                      (v_e1, s_e2))
                   else
                     (let v_e2 = eval vars vars_map tp VIO f2 in
                      (s_e1, v_e2))) in
       Pdt.apply2_reduce Proof.opt_equal vars
         (fun p1_opt p2_opt -> (do_iff p1_opt p2_opt pol)) expl1 expl2
    | Exists (x, tc, f) ->
       let vars_map = Map.add_exn vars_map ~key:x ~data:(Quantifier.Existential, pol) in
       let expl = eval (vars @ [x]) vars_map tp pol f in
       let expl =
         Pdt.hide_reduce Proof.opt_equal (vars @ [x])
           (fun p_opt -> do_exists_leaf x tc p_opt)
           (fun part -> Proof.Size.minp_list_somes (do_exists_node x tc part)) expl in
       (* traceln "EXISTS expl = %s" (Expl.opt_to_string expl); *)
       expl
    | Forall (x, tc, f) ->
       let vars_map = Map.add_exn vars_map ~key:x ~data:(Quantifier.Universal, pol) in
       let expl = eval (vars @ [x]) vars_map tp pol f in
       Pdt.hide_reduce Proof.opt_equal (vars @ [x])
         (fun p_opt -> do_forall_leaf x tc p_opt)
         (fun part -> Proof.Size.minp_list_somes (do_forall_node x tc part)) expl
    | Prev (i, f) ->
       if Int.equal tp 0 then
         (match pol with
          | SAT -> Pdt.Leaf None
          | VIO -> Pdt.Leaf (Some (V VPrev0)))
       else
          let expl = eval vars vars_map (tp - 1) pol f in
          let (cur_l, cur_u) = timestamp_interval prefix tp in 
          let (prev_l,prev_u) = timestamp_interval prefix (tp-1) in
          let expl =
            Pdt.apply1_reduce Proof.opt_equal vars
                       (fun p_opt -> do_prev i p_opt (prev_l,prev_u) (cur_l,cur_u) pol) expl
          in expl
    | Next (i, f) ->
      let expl = eval vars vars_map (tp+1) pol f in
      let (cur_l,cur_u) = timestamp_interval prefix tp in
      let (next_l,next_u) = timestamp_interval prefix (tp+1) in
      let expl =
            Pdt.apply1_reduce Proof.opt_equal vars
              (fun p_opt -> do_next i p_opt (cur_l,cur_u) (next_l,next_u) pol) expl
      in
      expl
    | Once (i, f) ->
                          (let (ts_l, ts_u) = timestamp_interval prefix tp in
                          let l = (match Interval.right i with
                            | None -> 0
                            | Some b -> ts_l - b )in
                          let r = (match ts_u with
                            | Infinity -> Infinity
                            | Finite ts_u -> Finite (ts_u - Interval.left i)) in
                            match pol with
                            | SAT -> let expl = once_sat tp (l,r) vars f tp (Pdt.Leaf None) vars_map in
                                      (* traceln "ONCE_SAT expl = %s" (Expl.opt_to_string expl); *)
                                      expl
                            | VIO -> let expl = Pdt.uneither (once_vio tp (l,r) vars f tp
                                                                (Pdt.Leaf (Either.second Fdeque.empty)) vars_map) in
                                      (* traceln "ONCE_VIO expl = %s" (Expl.opt_to_string expl); *)
                                      expl)
    | Eventually (i, f) ->  
                            let (ts_l, ts_u) = timestamp_interval prefix tp in
                            (match ts_u with
                            | Infinity -> failwith "infinite upper bound in Eventually"
                            | Finite ts_u ->
                              let l = ts_l + Interval.left i in
                              let r = match Interval.right i with
                              | None -> Infinity
                              | Some b -> Finite (ts_u + b) in
                              let candidates = future_candidate_tps prefix tp l r in
                            match pol with
                            | SAT -> let expl = eventually_sat tp candidates vars f tp (Pdt.Leaf None) vars_map in
                                     (* traceln "EVENTUALLY_SAT expl = %s" (Expl.to_string expl); *)
                                     expl
                            | VIO -> let expl = Pdt.uneither (eventually_vio tp candidates vars f tp
                                                                (Pdt.Leaf (Either.second Fdeque.empty)) vars_map) in
                                     (* traceln "EVENTUALLY_VIO expl = %s" (Expl.to_string expl); *)
                                     expl)
    | Historically (i, f) -> 
                          (let (ts_l, ts_u) = timestamp_interval prefix tp in
                          let l = (match Interval.right i with
                            | None -> 0
                            | Some b -> ts_l - b )in
                          let r = (match ts_u with
                            | Infinity -> Infinity
                            | Finite ts_u -> Finite (ts_u - Interval.left i)) in
                            let candidates = past_candidate_tps prefix tp l r in
                              match pol with
                              | SAT -> 
                                      (match r with
                                      | Infinity -> let expl = Pdt.uneither (historically_sat tp candidates vars f tp
                                                                  (Pdt.Leaf (Either.second Fdeque.empty)) vars_map)
                              in expl
                                      | Finite u -> if u < 0 then Pdt.Leaf (Some (Proof.S (SHistoricallyOut tp)))
                                      else
                                      let expl = Pdt.uneither (historically_sat tp candidates vars f tp
                                                                  (Pdt.Leaf (Either.second Fdeque.empty)) vars_map) in
                                       (* traceln "HISTORICALLY_SAT expl = %s" (Expl.to_string expl); *)
                                       expl)
                              | VIO -> let expl = historically_vio tp candidates vars f tp (Pdt.Leaf None) vars_map in
                                       (* traceln "HISTORICALLY_VIO expl = %s" (Expl.to_string expl); *)
                                       expl)
    | Always (i, f) -> let (ts_l, ts_u) = timestamp_interval prefix tp in
                            (match ts_u with
                            | Infinity -> failwith "infinite upper bound in Eventually"
                            | Finite ts_u ->
                              let l = ts_l + Interval.left i in
                              let r = match Interval.right i with
                              | None -> Infinity
                              | Some b -> Finite (ts_u + b) in
                              let candidates = future_candidate_tps prefix tp l r in
                        match pol with
                        | SAT -> let expl = Pdt.uneither (always_sat tp candidates vars f tp
                                                            (Pdt.Leaf (Either.second Fdeque.empty)) vars_map) in
                                 (* traceln "ALWAYS_SAT expl = %s" (Expl.to_string expl); *)
                                 expl
                        | VIO -> let expl = always_vio tp candidates vars f tp (Pdt.Leaf None) vars_map in
                                 (* traceln "ALWAYS_VIO expl = %s" (Expl.to_string expl); *)
                                 expl)
    | Since (i, f1, f2) -> (let (ts_l, ts_u) = timestamp_interval prefix tp in
                          let l = (match Interval.right i with
                            | None -> 0
                            | Some b -> ts_l - b )in
                          let r = (match ts_u with
                            | Infinity -> Infinity
                            | Finite ts_u -> Finite (ts_u - Interval.left i)) in
                            let candidates = past_candidate_tps prefix tp l r in
                            let min_cand = Option.value (Set.min_elt candidates) ~default:(tp + 1) in
                            match pol with
                            | SAT -> let expl = Pdt.uneither
                                                  (since_sat candidates min_cand vars f1 f2 tp
                                                     (Pdt.Leaf (Either.second Fdeque.empty)) vars_map) in
                                     (* traceln "SINCE_SAT expl = %s" (Expl.opt_to_string expl); *)
                                     expl
                            | VIO -> let expl =
                                       Pdt.uneither
                                         (since_vio tp candidates min_cand vars f1 f2 tp
                                            (Pdt.Leaf (Either.second Fdeque.empty)) vars_map) in
                                    (* traceln "SINCE_VIO  expl = %s"  (Expl.opt_to_string expl); *)
                                     expl)
    | Until (i, f1, f2) -> let (ts_l, ts_u) = timestamp_interval prefix tp in
                            (match ts_u with
                            | Infinity -> failwith "infinite upper bound in Eventually"
                            | Finite ts_u ->
                              let l = ts_l + Interval.left i in
                              let r = match Interval.right i with
                              | None -> Infinity
                              | Some b -> Finite (ts_u + b) in
                              let candidates = future_candidate_tps prefix tp l r in
                            match pol with
                            | SAT -> let expl = Pdt.uneither
                                                  (until_sat candidates vars f1 f2 tp
                                                     (Pdt.Leaf (Either.second Fdeque.empty)) vars_map) in
                                     (* traceln "UNTIL_SAT expl = %s" (Expl.to_string expl); *)
                                     expl
                            | VIO -> let expl =
                                       Pdt.uneither
                                         (until_vio tp candidates vars f1 f2 tp
                                            (Pdt.Leaf (Either.second (None, Fdeque.empty))) vars_map) in
                                     (* traceln "UNTIL_VIO (l=%d,r=%d) expl = %s" l r (Expl.to_string expl); *)
                                     expl)
  
  (* Once *)
  
  and once_sat cur_tp (l, r) vars f tp mexpl vars_map =
  if tp < 0 || (match r with Finite v -> v < 0 | Infinity -> false) then
    Pdt.apply1_reduce Proof.opt_equal vars (fun p_opt -> p_opt) mexpl
  else
    (let (ts_l, ts_u) = timestamp_interval prefix tp in
    if (match ts_u with Finite v -> v < l | Infinity -> false) then
      Pdt.apply1_reduce Proof.opt_equal vars (fun p_opt -> p_opt) mexpl
    else if (match r with Finite r -> ts_l > r | Infinity -> false) then
      once_sat cur_tp (l, r) vars f (tp - 1) mexpl vars_map
    else
      (if (match r with Finite v -> ts_l <= v | Infinity -> true) then
      (let expl = eval vars vars_map tp SAT f in
      let mexpl =
        Pdt.apply2_reduce Proof.opt_equal vars
          (fun sp_opt p_opt ->
            match p_opt with
            | None ->
                (match sp_opt with
                 | None -> None
                 | Some (Proof.S sp) -> Some (Proof.S (SOnce (cur_tp, sp)))
                 | _ -> raise (Invalid_argument "found V proof in S case"))
            | Some p -> Some p)
          expl mexpl
      in
      if stop vars vars_map mexpl SAT then mexpl
      else once_sat cur_tp (l, r) vars f (tp - 1) mexpl vars_map)
    else once_sat cur_tp (l, r) vars f (tp - 1) mexpl vars_map))


  and once_vio cur_tp (l,r) vars f tp mexpl vars_map =
    if tp < 0 then
      Pdt.apply1_reduce either_v_equal vars
        (function First p -> First p
                | Second vps -> Either.first (Some (Proof.V (Proof.VOnce (cur_tp, tp+1, vps))))) mexpl
    else
      (if (match r with | Finite v -> v < 0 | Infinity -> false) then
         Pdt.apply1_reduce either_v_equal vars
           (function First p -> First p
                   | Second _ -> Either.first (Some (Proof.V (VOnceOut cur_tp)))) mexpl
       else
         (let (ts_l,ts_u) = timestamp_interval prefix tp in
          if ts < l then
            (Pdt.apply1_reduce either_v_equal vars
               (function First p -> First p
                       | Second vps -> Either.first (Some (Proof.V (Proof.VOnce (cur_tp, tp+1, vps))))) mexpl)
          else
            (if ts <= r then
               (let expl = eval vars vars_map tp VIO f in
                let mexpl = Pdt.apply2_reduce either_v_equal vars
                              (fun vp_opt p_vps ->
                                match p_vps with
                                | First p -> First p
                                | Second vps ->
                                   (match vp_opt with
                                    | None -> Either.first None
                                    | Some (Proof.V vp) -> Either.second (Fdeque.enqueue_front vps vp)
                                    | _ -> raise (Invalid_argument "found S proof in V case")))
                              expl mexpl in
                if stop_either vars vars_map mexpl VIO then mexpl
                else once_vio cur_tp (l,r) vars f (tp-1) mexpl vars_map)
             else once_vio cur_tp (l,r) vars f (tp-1) mexpl vars_map)))
  (* Eventually *)
  and eventually_sat cur_tp candidates vars f tp mexpl vars_map =
    let terminate mexpl =
      Pdt.apply1_reduce Proof.opt_equal vars (fun p_opt -> p_opt) mexpl
    in
    let bin_search mexpl =
    match next_in_set candidates tp with
    | Some next_tp -> eventually_sat cur_tp candidates vars f next_tp mexpl vars_map
    | None -> terminate mexpl
  in
  if tp > prefix_max_tp prefix then
    terminate mexpl
  else if not (Set.mem candidates tp) then
    bin_search mexpl
    else
         (let expl = eval vars vars_map tp SAT f in
          let mexpl = Pdt.apply2_reduce Proof.opt_equal vars
                        (fun sp_opt p_opt ->
                          match p_opt with
                          | None -> (match sp_opt with
                                     | None -> None
                                     | Some (Proof.S sp) -> Some (Proof.S (SEventually (cur_tp, sp)))
                                     | _ -> raise (Invalid_argument "found V proof in S case"))
                          | Some p -> Some p) expl mexpl in
          if stop vars vars_map mexpl SAT then mexpl
          else eventually_sat cur_tp candidates vars f (tp+1) mexpl vars_map)
  and eventually_vio cur_tp candidates vars f tp mexpl vars_map =
      let terminate mexpl =
        Pdt.apply1_reduce either_v_equal vars
        (function First p -> First p
                | Second vps -> Either.first (Some (Proof.V (Proof.VEventually (cur_tp, tp-1, vps))))) mexpl
      in
      let bin_search mexpl =
          match next_in_set candidates tp with
          | Some next_tp -> eventually_vio cur_tp candidates vars f next_tp mexpl vars_map
          | None -> terminate mexpl
        in
      if tp > prefix_max_tp prefix then
          terminate mexpl
      else if not (Set.mem candidates tp) then
          bin_search mexpl
      else
         (let expl = eval vars vars_map tp VIO f in
          let mexpl = Pdt.apply2_reduce either_v_equal vars
                        (fun vp_opt p_vps ->
                          match p_vps with
                          | First p -> First p
                          | Second vps ->
                             (match vp_opt with
                              | None -> Either.first None
                              | Some (Proof.V vp) -> Either.second (Fdeque.enqueue_back vps vp)
                              | _ -> raise (Invalid_argument "found S proof in V case")))
                        expl mexpl in
          if stop_either vars vars_map mexpl VIO then mexpl
          else eventually_vio cur_tp candidates vars f (tp+1) mexpl vars_map)

  (* Historically *)
  and historically_sat cur_tp candidates vars f tp mexpl vars_map =
    let terminate mexpl =
      Pdt.apply1_reduce either_s_equal vars
        (function First p -> First p
                | Second sps -> Either.first (Some (Proof.S (Proof.SHistorically (cur_tp, tp+1, sps))))) mexpl
    in
    let bin_search mexpl =
      match prev_in_set candidates tp with
      | Some next_tp -> historically_sat cur_tp candidates vars f next_tp mexpl vars_map
      | None -> terminate mexpl
  in
    if tp < 0 then
      terminate mexpl
    else if not (Set.mem candidates tp) then
      bin_search mexpl
       else
               let expl = eval vars vars_map tp SAT f in
                let mexpl = Pdt.apply2_reduce either_s_equal vars
                              (fun sp_opt p_sps ->
                                match p_sps with
                                | First p -> First p
                                | Second sps ->
                                   (match sp_opt with
                                    | None -> Either.first None
                                    | Some (Proof.S sp) -> Either.second (Fdeque.enqueue_front sps sp)
                                    | _ -> raise (Invalid_argument "found V proof in S case")))
                              expl mexpl in
                if stop_either vars vars_map mexpl SAT then mexpl
             else historically_sat cur_tp candidates vars f (tp-1) mexpl vars_map
  and historically_vio cur_tp candidates vars f tp mexpl vars_map =
    let terminate mexpl =
      Pdt.apply1_reduce Proof.opt_equal vars (fun p_opt -> p_opt) mexpl
    in
    let bin_search mexpl =
      match prev_in_set candidates tp with
      | Some next_tp -> historically_vio cur_tp candidates vars f next_tp mexpl vars_map
      | None -> terminate mexpl
    in
    if tp < 0 then
      terminate mexpl
    else
       if not (Set.mem candidates tp) then
     bin_search mexpl
    else
            let expl = eval vars vars_map tp VIO f in
             let mexpl = Pdt.apply2_reduce Proof.opt_equal vars
                           (fun sp_opt p_opt ->
                             match p_opt with
                             | None -> (match sp_opt with
                                        | None -> None
                                        | Some (Proof.V vp) -> Some (Proof.V (VHistorically (cur_tp, vp)))
                                        | _ -> raise (Invalid_argument "found S proof in V case"))
                             | Some p -> Some p) expl mexpl in
             if stop vars vars_map mexpl VIO then mexpl
          else historically_vio cur_tp candidates vars f (tp-1) mexpl vars_map

  (* Always *)
  and always_sat cur_tp candidates vars f tp mexpl vars_map =
    let terminate mexpl =
      Pdt.apply1_reduce either_s_equal vars
        (function First p -> First p
                | Second sps -> Either.first (Some (Proof.S (Proof.SAlways (cur_tp, tp-1, sps))))) mexpl
    in
    let bin_search mexpl =
      match next_in_set candidates tp with
      | Some next_tp -> always_sat cur_tp candidates vars f next_tp mexpl vars_map
      | None -> terminate mexpl
    in
    if tp > prefix_max_tp prefix then
      terminate mexpl
    else if not (Set.mem candidates tp) then 
        bin_search mexpl
    else
         let expl = eval vars vars_map tp SAT f in
          let mexpl = Pdt.apply2_reduce either_s_equal vars
                        (fun sp_opt p_sps ->
                          match p_sps with
                          | First p -> First p
                          | Second sps ->
                             (match sp_opt with
                              | None -> Either.first None
                              | Some (Proof.S sp) -> Either.second (Fdeque.enqueue_back sps sp)
                              | _ -> raise (Invalid_argument "found V proof in S case")))
                        expl mexpl in
          if stop_either vars vars_map mexpl SAT then mexpl
       else always_sat cur_tp candidates vars f (tp+1) mexpl vars_map
  and always_vio cur_tp candidates vars f tp mexpl vars_map =
      if tp > prefix_max_tp prefix then
      Pdt.apply1_reduce Proof.opt_equal vars (fun p_opt -> p_opt) mexpl
      else if not (Set.mem candidates tp) then
        always_vio cur_tp candidates vars f (tp+1) mexpl vars_map
      else
         let expl = eval vars vars_map tp VIO f in
          let mexpl = Pdt.apply2_reduce Proof.opt_equal vars
                        (fun vp_opt p_opt ->
                          match p_opt with
                          | None -> (match vp_opt with
                                     | None -> None
                                     | Some (Proof.V vp) -> Some (Proof.V (VAlways (cur_tp, vp)))
                                     | _ -> raise (Invalid_argument "found S proof in V case"))
                          | Some p -> Some p) expl mexpl in
          if stop vars vars_map mexpl VIO then mexpl
       else always_vio cur_tp candidates vars f (tp+1) mexpl vars_map

  (* Since *)
  and since_sat candidates min_cand vars f1 f2 tp mexpl vars_map =
    if tp < min_cand  then
      Pdt.apply1_reduce either_s_equal vars
        (function First p -> First p
                | Second _ -> Either.first None) mexpl
    else if (Set.mem candidates tp) then
            let expl1 = eval vars vars_map tp SAT f1 in
             let expl2 = eval vars vars_map tp SAT f2 in
             let mexpl = Pdt.apply3_reduce either_s_equal vars
                           (fun sp1_opt sp2_opt p_sp1s ->
                             match p_sp1s with
                             | First p -> First p
                             | Second sp1s ->
                                (match sp1_opt, sp2_opt with
                                 | None, None -> Either.first None
                                 | Some (Proof.S sp1), None ->
                                    Either.second (Fdeque.enqueue_front sp1s sp1)
                                 | _, Some (Proof.S sp2) ->
                                    Either.first (Some (Proof.S (SSince (sp2, sp1s))))
                                 | _ -> raise (Invalid_argument "found V proof in S deque")))
                           expl1 expl2 mexpl in
             if stop_either vars vars_map mexpl SAT then mexpl
             else since_sat candidates min_cand vars f1 f2 (tp-1) mexpl vars_map
          else
            (let expl1 = eval vars vars_map tp SAT f1 in
             let mexpl = Pdt.apply2_reduce either_s_equal vars
                           (fun sp1_opt p_sp1s ->
                             match p_sp1s with
                             | First p -> First p
                             | Second sp1s ->
                                (match sp1_opt with
                                 | None -> Either.first None
                                 | Some (Proof.S sp1) ->
                                    Either.second (Fdeque.enqueue_front sp1s sp1)
                                 | _ -> raise (Invalid_argument "found V proof in S deque")))
                           expl1 mexpl in
             if stop_either vars vars_map mexpl SAT then mexpl
             else since_sat candidates min_cand vars f1 f2 (tp-1) mexpl vars_map)
  and since_vio cur_tp candidates min_cand vars f1 f2 tp mexpl vars_map =
    if tp < min_cand then
      Pdt.apply1_reduce either_v_equal vars
        (function First p -> First p
                | Second vp2s -> Either.first (Some (Proof.V (Proof.VSinceInf (cur_tp, tp+1, vp2s))))) mexpl
    else if (Set.mem candidates tp) then
               (let expl1 = eval vars vars_map tp VIO f1 in
                let expl2 = eval vars vars_map tp VIO f2 in
                let mexpl = Pdt.apply3_reduce either_v_equal vars
                              (fun vp1_opt vp2_opt p_vp2s ->
                                match p_vp2s with
                                | First p -> First p
                                | Second vp2s ->
                                   (match vp1_opt, vp2_opt with
                                    | None, Some (Proof.V vp2) ->
                                       Either.second (Fdeque.enqueue_front vp2s vp2)
                                    | Some (Proof.V vp1), Some (Proof.V vp2) ->
                                       Either.first
                                         (Some (Proof.V (VSince (cur_tp, vp1, Fdeque.enqueue_front vp2s vp2))))
                                    | _ ->
                                       Either.first None)) expl1 expl2 mexpl in
                since_vio cur_tp candidates min_cand vars f1 f2 (tp-1) mexpl vars_map)
             else
               (let expl1 = eval vars vars_map tp VIO f1 in
                let mexpl = Pdt.apply2_reduce either_v_equal vars
                              (fun vp1_opt p_vp2s ->
                                match p_vp2s with
                                  First p -> First p
                                | Second vp2s ->
                                   (match vp1_opt with
                                    | None -> Second vp2s
                                    | Some (Proof.V vp1) ->
                                       Either.first (Some (Proof.V (Proof.VSince (cur_tp, vp1, Fdeque.empty))))))
                              expl1 mexpl in
                since_vio cur_tp candidates min_cand vars f1 f2 (tp-1) mexpl vars_map)

  (* Until *)
  and until_sat candidates vars f1 f2 tp mexpl vars_map =
    if tp > prefix_max_tp prefix then
      Pdt.apply1_reduce either_s_equal vars
        (function First p -> First p
                | Second _ -> Either.first None) mexpl
    else if (Set.mem candidates tp) then
         let expl1 = eval vars vars_map tp SAT f1 in
          let expl2 = eval vars vars_map tp SAT f2 in
          let mexpl = Pdt.apply3_reduce either_s_equal vars
                        (fun sp1_opt sp2_opt p_sp1s ->
                          match p_sp1s with
                          | First p -> First p
                          | Second sp1s ->
                             (match sp1_opt, sp2_opt with
                              | None, None -> Either.first None
                              | Some (Proof.S sp1), None ->
                                 Either.second (Fdeque.enqueue_back sp1s sp1)
                              | _, Some (Proof.S sp2) ->
                                 Either.first (Some (Proof.S (SUntil (sp2, sp1s))))
                              | _ -> raise (Invalid_argument "found V proof in S deque")))
                        expl1 expl2 mexpl in
          if stop_either vars vars_map mexpl SAT then mexpl
          else until_sat candidates vars f1 f2 (tp+1) mexpl vars_map
       else
         (let expl1 = eval vars vars_map tp SAT f1 in
          let mexpl = Pdt.apply2_reduce either_s_equal vars
                        (fun sp1_opt p_sp1s ->
                          match p_sp1s with
                          | First p -> First p
                          | Second sp1s ->
                             (match sp1_opt with
                              | None -> Either.first None
                              | Some (Proof.S sp1) ->
                                 Either.second (Fdeque.enqueue_back sp1s sp1)
                              | _ -> raise (Invalid_argument "found V proof in S deque")))
                        expl1 mexpl in
          if stop_either vars vars_map mexpl SAT then mexpl
          else until_sat candidates vars f1 f2 (tp+1) mexpl vars_map)
  and until_vio cur_tp candidates vars f1 f2 tp mexpl vars_map =
    if tp >= prefix_max_tp prefix then
      Pdt.apply1_reduce either_v_equal2 vars
        (function First p -> First p
                | Second (_, vp2s) -> Either.first (Some (Proof.V (Proof.VUntilInf (cur_tp, tp-1, vp2s))))) mexpl
    else if (Set.mem candidates tp) then
         let expl1 = eval vars vars_map tp VIO f1 in
          let expl2 = eval vars vars_map tp VIO f2 in
          let mexpl = Pdt.apply3_reduce either_v_equal2 vars
                        (fun vp1_opt vp2_opt p_vp2s ->
                          match p_vp2s with
                          | First p -> First p
                          | Second (Some vp1, vp2s) ->
                             Either.first (Some (Proof.V (VUntil (cur_tp, vp1, vp2s))))
                          | Second (None, vp2s) ->
                             (match vp1_opt, vp2_opt with
                              | None, None -> Either.first None
                              | None, Some p ->
                                 Either.second (None, Fdeque.enqueue_back vp2s (Proof.unV p))
                              | Some p, None -> Either.first None
                              | Some p1, Some p2 ->
                                 Either.second (Some (Proof.unV p1), Fdeque.enqueue_back vp2s (Proof.unV p2))))
                        expl1 expl2 mexpl in
          if stop_either vars vars_map mexpl VIO then mexpl
          else until_vio cur_tp candidates vars f1 f2 (tp+1) mexpl vars_map
       else
         (let expl1 = eval vars vars_map tp VIO f1 in
          let mexpl = Pdt.apply2_reduce either_v_equal2 vars
                        (fun vp1_opt p_vp2s ->
                          match p_vp2s with
                            First p -> First p
                          | Second vp1_opt_vp2s ->
                             (match vp1_opt with
                              | None -> Second vp1_opt_vp2s
                              | Some (Proof.V vp1) ->
                                 Either.first (Some (Proof.V (Proof.VUntil (cur_tp, vp1, Fdeque.empty))))))
                        expl1 mexpl in
          if stop_either vars vars_map mexpl VIO then mexpl
          else until_vio cur_tp candidates vars f1 f2 (tp+1) mexpl vars_map) in
  eval [] (Map.empty (module String)) tp pol f


let send_headers http_flow =
  Eio.Flow.copy_string "HTTP/1.1 200 OK\n" http_flow;
  Eio.Flow.copy_string "Content-Type: text/event-stream\n" http_flow;
  Eio.Flow.copy_string "Cache-Control: no-cache\n" http_flow;
  Eio.Flow.copy_string "Connection: keep-alive\n" http_flow;
  Eio.Flow.copy_string "Access-Control-Allow-Origin: *\n\n" http_flow

let send_data json http_flow =
  Eio.Flow.copy_string "event: message\n" http_flow;
  Eio.Flow.copy_string (Printf.sprintf "data: %s\n\n" json) http_flow

let checker_interval_fix prefix =
  List.map (List.range 0 (prefix_max_tp prefix + 1)) ~f:(fun tp ->
    let db = db_at prefix tp in
    let ts_l, ts_u = timestamp_interval prefix tp in
    match ts_u with
    | Infinity ->
        [(ts_l, db)]
    | Finite ts_hi ->
        List.map (List.range ts_l (ts_hi + 1)) ~f:(fun ts ->
          (ts, db)))


let checker_interval_fix2 prefix =
  List.map (List.range 0 (prefix_max_tp prefix + 1)) ~f:(fun tp ->
    let ts = fst (timestamp_interval prefix tp) in
    let db = db_at prefix tp in
    (ts, db))
    
let interval_ts_option prefix tp =
  let interval = timestamp_interval prefix tp in
  match (snd interval) with
  | Infinity -> (fst interval, None)
  | Finite ts_u -> (fst interval,Some ts_u)



let read (mon: Argument.Monitor.t) r_buf r_sink prefix f pol mode vars vars_tt last_tp http_flow_opt =
    while true do
      let line = Eio.Buf_read.line r_buf in
      if !Etc.debug then traceln "Read emonitor line: %s" line;
      if String.equal line "Stop" then
        (match http_flow_opt with
        | None -> raise Exit
        | Some (http_flow) -> send_data "{\"disconnect\": true}" http_flow;
                              raise Exit);
      if Emonitor.is_verdict mon line then
        let (assignments) = Emonitor.to_tpts_assignments mon vars vars_tt line in
        if !Etc.debug then traceln "%s" (Etc.string_list_to_string ~sep:"\n" (List.map assignments ~f:((fun (_,_,v) -> Assignment.to_string v))));
        match http_flow_opt with
        | None ->
            (List.iter assignments ~f:(fun (tp,ts, v) ->
                (* Stdio.printf "expl = %s\n %d" (Expl.opt_to_string (explain !prefix v pol tp f))tp; *)
                let expl = Pdt.unsomes (explain !prefix v pol tp f ) in
                match mode with
                | Argument.Mode.Unverified -> Out.Plain.print (Explanation (tp, (interval_ts_option !prefix tp),v, expl))
                | Verified ->
                    let db = db_at !prefix tp in
                    let base = checker_interval_fix2 !prefix in
                    let ts_l, ts_u = timestamp_interval !prefix tp in
                    let pot_ts =
                      match ts_u with
                      | Finite ts_hi ->
                          if Int.equal ts_l ts_hi then
                            [ts_l]
                          else
                            List.range ts_l (ts_hi + 1)
                      | Infinity ->
                          [ts_l]
                    in
                    let bs = List.map pot_ts ~f:(fun ts ->
                      let (b, _, _) =
                      Checker_interface.check (List.mapi base ~f:(fun i (t,d) ->
                          if i = tp then (ts,db) else if i > tp then (max t ts, d) else (t,d))) v f (Pdt.unleaf expl) in b) in
                    Out.Plain.print (ExplanationCheck (tp, (interval_ts_option !prefix tp),v, expl, bs))
                | LaTeX -> Out.Plain.print (ExplanationLatex (tp, (interval_ts_option !prefix tp), v,  expl, f))
                | Debug ->
                    let db = db_at !prefix tp in
                    let base = checker_interval_fix2 !prefix in
                    let ts_l, ts_u = timestamp_interval !prefix tp in
                    let pot_ts =
                      match ts_u with
                      | Finite ts_hi ->
                          if Int.equal ts_l ts_hi then [ts_l]
                          else List.range ts_l (ts_hi + 1)
                      | Infinity ->
                          [ts_l]
                    in
                    let bs = List.map (pot_ts) ~f:(fun (ts) ->
                      let (b,_,_) = Checker_interface.check
                        (List.mapi base ~f:(fun i (t,d) ->
                          if i = tp then (ts,db) else if i > tp then (max t ts, d) else (t,d)))
                        v f (Pdt.unleaf expl) in b) in
                      let (_,c_e,c_trace) = Checker_interface.check (checker_interval_fix2 !prefix) v f (Pdt.unleaf expl) in
                    Out.Plain.print (ExplanationCheckDebug (tp, (interval_ts_option !prefix tp), v, expl, bs, c_e, c_trace))
                | DebugVis -> ()))
        | Some http_flow -> 
            List.iter assignments ~f:(fun (tp,ts, v) ->
            (let expl = if List.is_empty assignments then
                          Pdt.unsomes (explain !prefix (Map.empty (module String)) pol tp f)
                        else (Option.value_exn
                                (List.fold assignments ~init:None ~f:(fun expl (tp,ts,v) ->
                                    let pt = Expl.Pdt.unleaf (Pdt.unsomes (explain !prefix v pol tp f)) in
                                    Some(Expl.to_gui vars v pt expl)))) in
            (match mode with
              | Argument.Mode.Unverified ->
                let (ertp, lrtp) = (Expl.ertp expl, Expl.lrtp expl) in
                let tp_range = List.range ertp (lrtp + 1) in
                let json_ts prefix tp =
                Some (fst (timestamp_interval prefix tp)) in
                let json_dbs =
                List.mapi tp_range ~f:(fun i tp_i ->
                  Out.Json.db
                    (json_ts !prefix tp_i)
                    tp_i
                    i
                    (db_at !prefix tp_i)
                    f)
              in
                let json_expl_rows =
                List.mapi tp_range ~f:(fun _i tp_i ->
                  Out.Json.expl_row
                    (json_ts !prefix tp_i)
                    ertp
                    (if Int.equal tp tp_i then Some (f, expl) else None))
              in
                send_data (Out.Json.aggregate tp json_dbs json_expl_rows) http_flow 
              | Verified -> ()
              | LaTeX
                | Debug
                | DebugVis -> ()))) 
      else
        (match mon with
        (* Timelymon has no get_pos*)
        | TimelyMon | DejaVu -> ();
        | _ ->
        (* get_pos output to keep track of progress *)
            (if !Etc.debug then traceln "Read current progress";
            let tp = Emonitor.parse_prog_tp mon line in
            if Int.equal !last_tp tp && !writer_done  then (Eio.Flow.copy_string "Stop\n" r_sink));
          Fiber.yield ()
        )
    done


let write (mon: Argument.Monitor.t) w_sink stream prefix last_tp =
  let rec step pb_opt =
    match Other_parser.Event_stream.parse stream pb_opt mon with
    | Finished -> if !Etc.debug then traceln "Reached the end of event stream";
                  writer_done := true;
                  last_tp := prefix_max_tp !prefix;
                  (match mon with
                      | DejaVu
                      | TimelyMon -> Eio.Flow.close w_sink
                      | MonPoly | VeriMon ->
                            Eio.Flow.copy_string "> get_pos <\n" w_sink);
                  Fiber.yield ()
    | Skipped (pb, msg) -> if !Etc.debug then traceln "Skipped time-point due to: %S" msg;
                           Fiber.yield ();
                           step (Some(pb))
    | Processed pb -> (match pb.ts with
                      | Some ts -> 
                              if !Etc.debug then traceln "Processed event with time-stamp %d. Sending it to sink." ts;
                      | None -> 
                              if !Etc.debug then traceln "Processed event with missing time-stamp. Sending it to sink.");
                      Eio.Flow.copy_string (Emonitor.write_line mon (pb.tp, pb.ts, pb.db)) w_sink;
                      (match mon with
                      | TimelyMon | DejaVu -> ()
                      | MonPoly | VeriMon  ->
                            Eio.Flow.copy_string "> get_pos <\n" w_sink);  
                      let prefix_tp =
                      if !Etc.log_is_csv then
                        pb.tp
                      else
                        !last_tp + 1 in
                      last_tp := Int.max !last_tp prefix_tp;
                      insert_prefix !prefix prefix_tp pb.ts pb.db;
                      Fiber.yield ();
                      step (Some(pb)) 
    | Watermark w ->
                      (match mon with
                      | TimelyMon -> Eio.Flow.copy_string (Timelylog.watermark_line w) w_sink
                      | MonPoly | VeriMon | DejaVu -> ());
                      Fiber.yield ();
                      step pb_opt
  in
  step None
let run_emonitor mon mon_path sig_path f_path r_sink w_source proc_mgr extra_args =
  let f_realpath = Filename_unix.realpath (Eio.Path.native_exn f_path) in
  let args = Emonitor.args mon ~mon_path ?sig_path ~f_path:f_realpath in
  let cmd = args @ extra_args in
  if !Etc.debug then traceln "Running process with: %s" (Etc.string_list_to_string ~sep:" " cmd);
  Fun.protect
    ~finally:(fun () ->
      Eio.Flow.close r_sink;   (* close parent write-end of child stdout/stderr *)
      Eio.Flow.close w_source) (* close parent read-end of child stdin *)
    (fun () ->
      Eio.Process.run ~stdin:w_source ~stdout:r_sink ~stderr:r_sink proc_mgr cmd)


let exec_fibers mon mon_path sig_path f_path r_sink w_source w_sink r_buf proc_mgr
      extra_args stream prefix last_tp f pol mode vars vars_tt http_flow_opt =
  try
    Fiber.all
      [ (* Spawn thread with external monitor process *)
        (fun () -> run_emonitor mon mon_path sig_path f_path r_sink w_source proc_mgr extra_args);
        (* External monitor I/O management *)
        (fun () -> if !Etc.debug then traceln "Writing lines to emonitor's stdin...";
                   write mon w_sink stream prefix last_tp);
        (fun () -> if !Etc.debug then traceln "Reading lines from emonitor's stdout...";
                   read mon r_buf r_sink prefix f pol mode vars vars_tt last_tp http_flow_opt)]
  with Exit -> if !Etc.debug then traceln "Reached the end of the log file"

(* sig_path is only passed as a parameter when either MonPoly or VeriMon is the external monitor *)
let exec interf mon ~mon_path ?sig_path ~formula_file stream f pref mode extra_args =
  let pol = Polarity.of_pref pref in
  let vars = Set.elements (Formula.fv f) in
  let vars_tt = List.map vars ~f:(fun x -> Formula.var_tt x f) in
  let ( / ) = Eio.Path.( / ) in
  Eio_main.run @@ fun env ->
    (* Formula conversion *)
    let f_path = Eio.Stdenv.cwd env / ("tmp/" ^ formula_file) in
    if !Etc.debug then traceln "Saving formula in %a" Eio.Path.pp f_path;
    Eio.Path.save ~create:(`Or_truncate 0o644) f_path (Formula.convert mon f);
    (* Instantiate process/domain managers *)
    let proc_mgr = Eio.Stdenv.process_mgr env in
    let domain_mgr = Eio.Stdenv.domain_mgr env in
    Switch.run (fun sw ->
      (* source and sink of emonitor's stdin *)
      let w_source, w_sink = Eio.Process.pipe ~sw proc_mgr in
      (* source and sink of emonitor's stdout *)
      let r_source, r_sink = Eio.Process.pipe ~sw proc_mgr in
      let r_buf = Eio.Buf_read.of_flow r_source ~initial_size:100 ~max_size:1_000_000 in
      (* accumulated prefix ref *)
      let prefix =
        ref {
          obs_seq = [IInt (0, 0)];
          dbs = Hashtbl.create (module Int);
        } 
      in
      (* last time-point in the stream *)
      let last_tp = ref (-1) in
      match interf with
      | Argument.Interface.CLI ->
         exec_fibers mon mon_path sig_path f_path r_sink w_source w_sink r_buf proc_mgr
           extra_args stream prefix last_tp f pol mode vars vars_tt None
      | GUI -> let net = Eio.Stdenv.net env in
               let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 31415) in
               let s = Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net addr in
               if !Etc.debug then traceln "Starting server...";
               let http_flow, _ = Eio.Net.accept ~sw s in
               if !Etc.debug then traceln "Established connection with client";
               send_headers http_flow;
               send_data (Out.Json.table_columns f) http_flow;
               (* while true do *)
               (*   UnixLabels.sleep 5; *)
               (*   send_data (Out.Json.table_columns f) http_flow *)
               (*     done *)
               exec_fibers mon mon_path sig_path f_path r_sink w_source w_sink r_buf proc_mgr
                 extra_args stream prefix last_tp f pol mode vars vars_tt (Some(http_flow)))