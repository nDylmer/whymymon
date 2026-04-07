(*******************************************************************)
(*     This is part of WhyMon, and it is distributed under the     *)
(*     terms of the GNU Lesser General Public License version 3    *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2023:                                                *)
(*  Dmitriy Traytel (UCPH)                                         *)
(*  Leonardo Lima (UCPH)                                           *)
(*******************************************************************)

open Base
open Pred

module Fdeque = Core.Fdeque

module Part = struct

  type sub = (Dom.t, Dom.comparator_witness) Setc.t

  type 'a t = (sub * 'a) list

  let random_empty_set = Set.empty (module String)

  let trivial e = [(Setc.univ (module Dom), e)]

  let hd part = snd (List.hd_exn part)

  let length part = List.length part

  let rev part = List.rev part

  let map part f = List.map part ~f:(fun (s, e) -> (s, f e))

  let map2 part f = List.map part ~f:(fun (s, e) -> f (s, e))

  let map2_list = map2

  let fold_left part init f = List.fold_left part ~init:init ~f:(fun acc (_, e) -> f acc e)

  let fold_map_list part init f = List.fold_map part ~init:init ~f

  let filter part f = List.filter part ~f:(fun (_, e) -> f e)

  let filter_map part f = List.filter_map part ~f:(fun (s, e) -> f (s, e))

  let exists part f = List.exists part ~f:(fun (_, e) -> f e)

  let unsomes part = List.map part ~f:(fun (s, e) -> (s, Option.value_exn e))

  let for_all part f = List.for_all part ~f:(fun (_, e) -> f e)

  let find_map part f = List.find_map part ~f:(fun (_, e) -> f e)

  let find_remove part d =
    let sub = Setc.Finite (Set.of_list (module Dom) [d]) in
    let expl_opt = List.find part ~f:(fun (s, _) ->
                       Setc.equal s sub) in
    match expl_opt with
    | None -> None
    | Some(_, e) -> Some(List.filter part ~f:(fun (s, e) -> not (Setc.equal s sub)), e)

  let values part = List.map part ~f:(fun (_, e) -> e)

  let of_list l =
    let univ_union = Setc.is_univ
                       (List.fold l ~init:(Setc.Finite (Set.of_list (module Dom) []))
                          ~f:(fun acc (s, _) -> Setc.union acc s)) in
    if univ_union then l
    else raise (Invalid_argument (Printf.sprintf "the union of subsets does not amount to a partition"))

  let equal part part' eq =
    let equal_fst = match List.for_all2 (List.map part ~f:fst) (List.map part' ~f:fst) ~f:Setc.equal with
      | Ok b -> b
      | Unequal_lengths -> false in
    let equal_snd = match List.for_all2 (List.map part ~f:snd) (List.map part' ~f:snd) ~f:eq with
      | Ok b -> b
      | Unequal_lengths -> false in
    equal_fst && equal_snd

  let tabulate ds f z =
    (Setc.Complement ds, z) ::
      (Set.fold ds ~init:[] ~f:(fun acc d -> (Setc.Finite (Set.of_list (module Dom) [d]), f d) :: acc))

  let rec merge2 f part1 part2 = match part1, part2 with
    | [], _ -> []
    | (sub1, v1) :: part1, part2 ->
       let part12 = List.filter_map part2
                      ~f:(fun (sub2, v2) ->
                        (if not (Setc.is_empty (Setc.inter sub1 sub2))
                         then Some (Setc.inter sub1 sub2, f v1 v2) else None)) in
       let part2not1 = List.filter_map part2
                         ~f:(fun (sub2, v2) ->
                           (if not (Setc.is_empty (Setc.diff sub2 sub1))
                            then Some (Setc.diff sub2 sub1, v2) else None)) in
       part12 @ (merge2 f part1 part2not1)

  let merge3 f part1 part2 part3 = match part1, part2, part3 with
    | [], _ , _
      | _ , [], _
      | _ , _ , [] -> raise (Invalid_argument "one of the partitions is empty")
    | part1, part2, part3 ->
       merge2 (fun pt3 f' -> f' pt3) part3 (merge2 f part1 part2)

  let split_prod part = (map part fst, map part snd)

  let split_list part =
    let subs = List.map part ~f:fst in
    let vs = List.map part ~f:snd in
    List.map (Option.value_exn (List.transpose vs)) ~f:(List.zip_exn subs)

  let rec el_to_string indent var f (sub, v) =
    Printf.sprintf "%s%s ∈ %s\n\n%s" indent (Term.value_to_string var) (Setc.to_string sub) (f v)

  let to_string indent var f = function
    | [] -> indent ^ "❮ · ❯"
    | [x] -> indent ^ "❮\n\n" ^ (el_to_string indent var (f indent) x) ^ "\n" ^ indent ^ "❯\n"
    | xs -> List.fold_left xs ~init:(indent ^ "❮\n\n")
              ~f:(fun s el -> s ^ (el_to_string indent var (f indent) el) ^ "\n\n") ^ indent ^ "❯\n"

  (* dedup related *)
  let dedup p_eq part =
    let rec aux acc (s,v) =
      match acc with
      | [] -> [(s,v)]
      | (t,u) :: acc -> if p_eq u v then (Setc.union s t, u) :: acc
                        else (t,u) :: aux acc (s,v) in
    let rec dedup_rec part acc =
      match part with
      | [] -> acc
      | (s,v) :: part -> let acc' = aux acc (s,v) in
                         dedup_rec part acc' in
    dedup_rec part []

  let sort = function
    | [] -> []
    | (s1,v1)::part ->
       (s1,v1)::(List.rev(List.sort part ~compare:(fun (s,_) (s',_) ->
                              Dom.compare_t
                                (Option.value_exn (Setc.min_elt s))
                                (Option.value_exn (Setc.min_elt s')))))

  let map_dedup p_eq part f = dedup p_eq (map part f)

  let map2_dedup p_eq part f = dedup p_eq (map2 part f)

  let tabulate_dedup p_eq ds f z = dedup p_eq (tabulate ds f z)

  let merge2_dedup p_eq f part1 part2 = dedup p_eq (merge2 f part1 part2)

  let merge3_dedup p_eq f part1 part2 part3 = dedup p_eq (merge3 f part1 part2 part3)

  let split_prod_dedup p_eq part =
    let part1, part2 = split_prod part in
    (dedup p_eq part1, dedup p_eq part2)

  let split_list_dedup p_eq part = List.map (split_list part) ~f:(dedup p_eq)

end


module Proof = struct

  type sp =
    | STT of int
    | SEqConst of int * string * Dom.t
    | SPred of int * string * Term.t list
    | SNeg of vp
    | SOrL of sp
    | SOrR of sp
    | SAnd of sp * sp
    | SImpL of vp
    | SImpR of sp
    | SIffSS of sp * sp
    | SIffVV of vp * vp
    | SExists of string * Dom.t * sp
    | SForall of string * (sp Part.t)
    | SPrev of sp
    | SNext of sp
    | SOnce of int * sp
    | SEventually of int * sp
    | SHistorically of int * int * sp Fdeque.t
    | SHistoricallyOut of int
    | SAlways of int * int * sp Fdeque.t
    | SSince of sp * sp Fdeque.t
    | SUntil of sp * sp Fdeque.t
  and vp =
    | VFF of int
    | VEqConst of int * string * Dom.t
    | VPred of int * string * Term.t list
    | VNeg of sp
    | VOr of vp * vp
    | VAndL of vp
    | VAndR of vp
    | VImp of sp * vp
    | VIffSV of sp * vp
    | VIffVS of vp * sp
    | VExists of string * (vp Part.t)
    | VForall of string * Dom.t * vp
    | VPrev of vp
    | VPrev0
    | VPrevOutL of int
    | VPrevOutR of int
    | VNext of vp
    | VNextOutL of int
    | VNextOutR of int
    | VOnceOut of int
    | VOnce of int * int * vp Fdeque.t
    | VEventually of int * int * vp Fdeque.t
    | VHistorically of int * vp
    | VAlways of int * vp
    | VSinceOut of int
    | VSince of int * vp * vp Fdeque.t
    | VSinceInf of int * int * vp Fdeque.t
    | VUntil of int * vp * vp Fdeque.t
    | VUntilInf of int * int * vp Fdeque.t

  type t = S of sp | V of vp

  let rec s_equal x y = match x, y with
    | STT tp, STT tp' -> Int.equal tp tp'
    | SEqConst (tp, x, c), SEqConst (tp', x', c') ->
       Int.equal tp tp' && String.equal x x' && Dom.equal c c'
    | SPred (tp, r, terms), SPred (tp', r', terms') ->
       Int.equal tp tp' && String.equal r r' &&
         Int.equal (List.length terms) (List.length terms') &&
           List.for_all2_exn terms terms' ~f:(fun t1 t2 -> Term.equal t1 t2)
    | SNeg vp, SNeg vp'
      | SImpL vp, SImpL vp' -> v_equal vp vp'
    | SImpR sp, SImpR sp'
      | SOrL sp, SOrL sp'
      | SOrR sp, SOrR sp'
      | SPrev sp, SPrev sp'
      | SNext sp, SNext sp' -> s_equal sp sp'
    | SAnd (sp1, sp2), SAnd (sp1', sp2')
      | SIffSS (sp1, sp2), SIffSS (sp1', sp2') -> s_equal sp1 sp1' && s_equal sp2 sp2'
    | SIffVV (vp1, vp2), SIffVV (vp1', vp2') -> v_equal vp1 vp1' && v_equal vp2 vp2'
    | SExists (x, d, sp), SExists (x', d', sp') -> String.equal x x' && Dom.equal d d' && s_equal sp sp'
    | SForall (x, part), SForall (x', part') ->
       String.equal x x' && Int.equal (Part.length part) (Part.length part') &&
         List.for_all2_exn part part' ~f:(fun (s, p) (s', p') ->
             Setc.equal s s' && s_equal p p')
    | SOnce (tp, sp), SOnce (tp', sp')
      | SEventually (tp, sp), SEventually (tp', sp') -> Int.equal tp tp' && s_equal sp sp'
    | SHistoricallyOut tp, SHistoricallyOut tp' -> Int.equal tp tp'
    | SHistorically (tp, lrtp, sps), SHistorically (tp', li', sps') ->
       Int.equal tp tp' && Int.equal lrtp li' &&
         Int.equal (Fdeque.length sps) (Fdeque.length sps') &&
           Etc.fdeque_for_all2_exn sps sps' ~f:(fun sp sp' -> s_equal sp sp')
    | SAlways (tp, htp, sps), SAlways (tp', hi', sps') ->
       Int.equal tp tp' && Int.equal htp hi' &&
         Int.equal (Fdeque.length sps) (Fdeque.length sps') &&
           Etc.fdeque_for_all2_exn sps sps' ~f:(fun sp sp' -> s_equal sp sp')
    | SSince (sp2, sp1s), SSince (sp2', sp1s')
      | SUntil (sp2, sp1s), SUntil (sp2', sp1s') ->
       s_equal sp2 sp2' && Int.equal (Fdeque.length sp1s) (Fdeque.length sp1s') &&
         Etc.fdeque_for_all2_exn sp1s sp1s' ~f:(fun sp1 sp1' -> s_equal sp1 sp1')
    | _ -> false
  and v_equal x y = match x, y with
    | VFF tp, VFF tp' -> Int.equal tp tp'
    | VEqConst (tp, x, c), VEqConst (tp', x', c') -> Int.equal tp tp' && String.equal x x' && Dom.equal c c'
    | VPred (tp, r, terms), VPred (tp', r', terms') ->
       Int.equal tp tp' && String.equal r r' &&
         Int.equal (List.length terms) (List.length terms') &&
           List.for_all2_exn terms terms' ~f:(fun t1 t2 -> Term.equal t1 t2)
    | VNeg sp, VNeg sp' -> s_equal sp sp'
    | VAndL vp, VAndL vp'
      | VAndR vp, VAndR vp'
      | VPrev vp, VPrev vp'
      | VNext vp, VNext vp' -> v_equal vp vp'
    | VOr (vp1, vp2), VOr (vp1', vp2') -> v_equal vp1 vp1' && v_equal vp2 vp2'
    | VImp (sp1, vp2), VImp (sp1', vp2')
      | VIffSV (sp1, vp2), VIffSV (sp1', vp2') -> s_equal sp1 sp1' && v_equal vp2 vp2'
    | VIffVS (vp1, sp2), VIffVS (vp1', sp2') -> v_equal vp1 vp1' && s_equal sp2 sp2'
    | VExists (x, part), VExists (x', part') ->
       String.equal x x' && Int.equal (Part.length part) (Part.length part') &&
         List.for_all2_exn part part' ~f:(fun (s, p) (s', p') ->
             Setc.equal s s' && v_equal p p')
    | VForall (x, d, vp), VForall (x', d', vp') -> String.equal x x' && Dom.equal d d' && v_equal vp vp'
    | VPrev0, VPrev0 -> true
    | VPrevOutL tp, VPrevOutL tp'
      | VPrevOutR tp, VPrevOutR tp'
      | VNextOutL tp, VNextOutL tp'
      | VNextOutR tp, VNextOutR tp'
      | VOnceOut tp, VOnceOut tp'
      | VSinceOut tp, VSinceOut tp' -> Int.equal tp tp'
    | VOnce (tp, lrtp, vps), VOnce (tp', li', vps') ->
       Int.equal tp tp' && Int.equal lrtp li' &&
         Int.equal (Fdeque.length vps) (Fdeque.length vps') &&
           Etc.fdeque_for_all2_exn vps vps' ~f:(fun vp vp' -> v_equal vp vp')
    | VEventually (tp, htp, vps), VEventually (tp', hi', vps') ->
       Int.equal tp tp' && Int.equal htp hi' &&
         Int.equal (Fdeque.length vps) (Fdeque.length vps') &&
           Etc.fdeque_for_all2_exn vps vps' ~f:(fun vp vp' -> v_equal vp vp')
    | VHistorically (tp, vp), VHistorically (tp', vp')
      | VAlways (tp, vp), VAlways (tp', vp') -> Int.equal tp tp'
    | VSince (tp, vp1, vp2s), VSince (tp', vp1', vp2s')
      | VUntil (tp, vp1, vp2s), VUntil (tp', vp1', vp2s') ->
       Int.equal tp tp' && v_equal vp1 vp1' &&
         Int.equal (Fdeque.length vp2s) (Fdeque.length vp2s') &&
           Etc.fdeque_for_all2_exn vp2s vp2s' ~f:(fun vp2 vp2' -> v_equal vp2 vp2')
    | VSinceInf (tp, lrtp, vp2s), VSinceInf (tp', li', vp2s') ->
       Int.equal tp tp' && Int.equal lrtp li' &&
         Int.equal (Fdeque.length vp2s) (Fdeque.length vp2s') &&
           Etc.fdeque_for_all2_exn vp2s vp2s' ~f:(fun vp2 vp2' -> v_equal vp2 vp2')
    | VUntilInf (tp, htp, vp2s), VUntilInf (tp', hi', vp2s') ->
       Int.equal tp tp' && Int.equal htp hi' &&
         Int.equal (Fdeque.length vp2s) (Fdeque.length vp2s') &&
           Etc.fdeque_for_all2_exn vp2s vp2s' ~f:(fun vp2 vp2' -> v_equal vp2 vp2')
    | _ -> false

  let equal x y = match x, y with
    | S sp, S sp' -> s_equal sp sp'
    | V vp, V vp' -> v_equal vp vp'
    | _ -> false

  let opt_s_equal x y = match x, y with
    | None, None -> true
    | Some _, None -> false
    | None, Some _ -> false
    | Some x, Some y -> s_equal x y

  let opt_v_equal x y = match x, y with
    | None, None -> true
    | Some _, None -> false
    | None, Some _ -> false
    | Some x, Some y -> v_equal x y

  let opt_equal x y = match x, y with
    | None, None -> true
    | Some _, None -> false
    | None, Some _ -> false
    | Some x, Some y -> equal x y

  let unS = function
    | S sp -> sp
    | _ -> raise (Invalid_argument "unS is not defined for V proofs")

  let unV = function
    | V vp -> vp
    | _ -> raise (Invalid_argument "unV is not defined for S proofs")

  let opt_unS = function
    | Some (S sp) -> sp
    | _ -> raise (Invalid_argument "opt_unS is not defined for None or V proofs")

  let opt_unV = function
    | Some (V vp) -> vp
    | _ -> raise (Invalid_argument "opt_unV is not defined for None or S proofs")

  let isS = function
    | S _ -> true
    | V _ -> false

  let isV = function
    | S _ -> false
    | V _ -> true

  let opt_isS = function
    | None -> false
    | Some p -> isS p

  let opt_isV = function
    | None -> false
    | Some p -> isV p

  let s_append sp sp1 = match (unS sp) with
    | SSince (sp2, sp1s) -> S (SSince (sp2, Fdeque.enqueue_back sp1s (unS sp1)))
    | SUntil (sp2, sp1s) -> S (SUntil (sp2, Fdeque.enqueue_back sp1s (unS sp1)))
    | _ -> raise (Invalid_argument "sappend is not defined for this sp")

  let v_append vp vp2 = match (unV vp) with
    | VSince (tp, vp1, vp2s) -> V (VSince (tp,  vp1, Fdeque.enqueue_back vp2s (unV vp2)))
    | VSinceInf (tp, etp, vp2s) -> V (VSinceInf (tp, etp, Fdeque.enqueue_back vp2s (unV vp2)))
    | VUntil (tp, vp1, vp2s) -> V (VUntil (tp, vp1, Fdeque.enqueue_back vp2s (unV vp2)))
    | VUntilInf (tp, lrtp, vp2s) -> V (VUntilInf (tp, lrtp, Fdeque.enqueue_back vp2s (unV vp2)))
    | _ -> raise (Invalid_argument "vappend is not defined for this vp")

  let s_drop = function
    | SUntil (sp2, sp1s) -> (match Fdeque.drop_front sp1s with
                             | None -> None
                             | Some(sp1s') -> Some (SUntil (sp2, sp1s')))
    | _ -> raise (Invalid_argument "sdrop is not defined for this sp")

  let v_drop = function
    | VUntil (tp, vp1, vp2s) -> (match Fdeque.drop_front vp2s with
                                 | None -> None
                                 | Some(vp2s') -> Some (VUntil (tp, vp1, vp2s')))
    | VUntilInf (tp, lrtp, vp2s) -> (match Fdeque.drop_front vp2s with
                                    | None -> None
                                    | Some(vp2s') -> Some (VUntilInf (tp, lrtp, vp2s)))
    | _ -> raise (Invalid_argument "vdrop is not defined for this vp")

  let rec s_at = function
    | STT tp -> tp
    | SEqConst (tp, _, _) -> tp
    | SPred (tp, _, _) -> tp
    | SNeg vp -> v_at vp
    | SOrL sp1 -> s_at sp1
    | SOrR sp2 -> s_at sp2
    | SAnd (sp1, _) -> s_at sp1
    | SImpL vp1 -> v_at vp1
    | SImpR sp2 -> s_at sp2
    | SIffSS (sp1, _) -> s_at sp1
    | SIffVV (vp1, _) -> v_at vp1
    | SExists (_, _, sp) -> s_at sp
    | SForall (_, part) -> s_at (Part.hd part)
    | SPrev sp -> s_at sp + 1
    | SNext sp -> s_at sp - 1
    | SOnce (tp, _) -> tp
    | SEventually (tp, _) -> tp
    | SHistorically (tp, _, _) -> tp
    | SHistoricallyOut tp -> tp
    | SAlways (tp, _, _) -> tp
    | SSince (sp2, sp1s) -> if Fdeque.is_empty sp1s then s_at sp2
                            else s_at (Fdeque.peek_back_exn sp1s)
    | SUntil (sp2, sp1s) -> if Fdeque.is_empty sp1s then s_at sp2
                            else s_at (Fdeque.peek_front_exn sp1s)
  and v_at = function
    | VFF tp -> tp
    | VEqConst (tp, _, _) -> tp
    | VPred (tp, _, _) -> tp
    | VNeg sp -> s_at sp
    | VOr (vp1, _) -> v_at vp1
    | VAndL vp1 -> v_at vp1
    | VAndR vp2 -> v_at vp2
    | VImp (sp1, _) -> s_at sp1
    | VIffSV (sp1, _) -> s_at sp1
    | VIffVS (vp1, _) -> v_at vp1
    | VExists (_, part) -> v_at (Part.hd part)
    | VForall (_, _, vp) -> v_at vp
    | VPrev vp -> v_at vp + 1
    | VPrev0 -> 0
    | VPrevOutL tp -> tp
    | VPrevOutR tp -> tp
    | VNext vp -> v_at vp - 1
    | VNextOutL tp -> tp
    | VNextOutR tp -> tp
    | VOnceOut tp -> tp
    | VOnce (tp, _, _) -> tp
    | VEventually (tp, _, _) -> tp
    | VHistorically (tp, _) -> tp
    | VAlways (tp, _) -> tp
    | VSinceOut tp -> tp
    | VSince (tp, _, _) -> tp
    | VSinceInf (tp, _, _) -> tp
    | VUntil (tp, _, _) -> tp
    | VUntilInf (tp, _, _) -> tp

  let p_at = function
    | S sp -> s_at sp
    | V vp -> v_at vp

  let opt_p_at = function
    | None -> None
    | Some p -> Some (p_at p)

  let cmp f p1 p2 = f p1 <= f p2

  let rec s_to_string indent p =
    let indent' = (String.make 4 ' ') ^ indent in
    match p with
    | STT i -> Printf.sprintf "%strue{%d}" indent i
    | SEqConst (tp, x, c) -> Printf.sprintf "%sSEqConst(%d, %s, %s)" indent tp x (Dom.to_string c)
    | SPred (tp, r, trms) -> Printf.sprintf "%sSPred(%d, %s, [%s])" indent tp r (Term.list_to_string trms)
    | SNeg vp -> Printf.sprintf "%sSNeg{%d}\n%s" indent (s_at p) (v_to_string indent' vp)
    | SOrL sp1 -> Printf.sprintf "%sSOrL{%d}\n%s" indent (s_at p) (s_to_string indent' sp1)
    | SOrR sp2 -> Printf.sprintf "%sSOrR{%d}\n%s" indent (s_at p) (s_to_string indent' sp2)
    | SAnd (sp1, sp2) -> Printf.sprintf "%sSAnd{%d}\n%s\n%s" indent (s_at p)
                           (s_to_string indent' sp1) (s_to_string indent' sp2)
    | SImpL vp1 -> Printf.sprintf "%sSImpL{%d}\n%s" indent (s_at p) (v_to_string indent' vp1)
    | SImpR sp2 -> Printf.sprintf "%sSImpR{%d}\n%s" indent (s_at p) (s_to_string indent' sp2)
    | SIffSS (sp1, sp2) -> Printf.sprintf "%sSIffSS{%d}\n%s\n%s" indent (s_at p)
                             (s_to_string indent' sp1) (s_to_string indent' sp2)
    | SIffVV (vp1, vp2) -> Printf.sprintf "%sSIffVV{%d}\n%s\n%s" indent (s_at p)
                             (v_to_string indent' vp1) (v_to_string indent' vp2)
    | SExists (x, d, sp) -> Printf.sprintf "%sSExists{%d}{%s=%s}\n%s\n" indent (s_at p)
                              x (Dom.to_string d) (s_to_string indent' sp)
    | SForall (x, part) -> Printf.sprintf "%sSForall{%d}{%s}\n\n%s\n" indent (s_at (SForall (x, part)))
                             x (Part.to_string indent' (Var x) s_to_string part)
    | SPrev sp -> Printf.sprintf "%sSPrev{%d}\n%s" indent (s_at p) (s_to_string indent' sp)
    | SNext sp -> Printf.sprintf "%sSNext{%d}\n%s" indent (s_at p) (s_to_string indent' sp)
    | SOnce (_, sp) -> Printf.sprintf "%sSOnce{%d}\n%s" indent (s_at p) (s_to_string indent' sp)
    | SEventually (_, sp) -> Printf.sprintf "%sSEventually{%d}\n%s" indent (s_at p)
                               (s_to_string indent' sp)
    | SHistorically (_, etp, sps) -> Printf.sprintf "%sSHistorically{%d}{%d}\n%s" indent (s_at p) etp
                                       (Etc.deque_to_string indent' s_to_string sps)
    | SHistoricallyOut i -> Printf.sprintf "%sSHistoricallyOut{%d}" indent i
    | SAlways (_, lrtp, sps) -> Printf.sprintf "%sSAlways{%d}{%d}\n%s" indent (s_at p) lrtp
                                 (Etc.deque_to_string indent' s_to_string sps)
    | SSince (sp2, sp1s) -> Printf.sprintf "%sSSince{%d}\n%s\n%s" indent (s_at p) (s_to_string indent' sp2)
                              (Etc.deque_to_string indent' s_to_string sp1s)
    | SUntil (sp2, sp1s) -> Printf.sprintf "%sSUntil{%d}\n%s\n%s" indent (s_at p)
                              (Etc.deque_to_string indent' s_to_string sp1s) (s_to_string indent' sp2)
  and v_to_string indent p =
    let indent' = "    " ^ indent in
    match p with
    | VFF i -> Printf.sprintf "%sfalse{%d}" indent i
    | VEqConst (tp, x, c) -> Printf.sprintf "%sVEqConst(%d, %s, %s)" indent tp x (Dom.to_string c)
    | VPred (tp, r, trms) -> Printf.sprintf "%sVPred(%d, %s, [%s])" indent tp r (Term.list_to_string trms)
    | VNeg sp -> Printf.sprintf "%sVNeg{%d}\n%s" indent (v_at p) (s_to_string indent' sp)
    | VOr (vp1, vp2) -> Printf.sprintf "%sVOr{%d}\n%s\n%s" indent (v_at p) (v_to_string indent' vp1) (v_to_string indent' vp2)
    | VAndL vp1 -> Printf.sprintf "%sVAndL{%d}\n%s" indent (v_at p) (v_to_string indent' vp1)
    | VAndR vp2 -> Printf.sprintf "%sVAndR{%d}\n%s" indent (v_at p) (v_to_string indent' vp2)
    | VImp (sp1, vp2) -> Printf.sprintf "%sVImp{%d}\n%s\n%s" indent (v_at p) (s_to_string indent' sp1) (v_to_string indent' vp2)
    | VIffSV (sp1, vp2) -> Printf.sprintf "%sVIffSV{%d}\n%s\n%s" indent (v_at p) (s_to_string indent' sp1)
                             (v_to_string indent' vp2)
    | VIffVS (vp1, sp2) -> Printf.sprintf "%sVIffVS{%d}\n%s\n%s" indent (v_at p) (v_to_string indent' vp1)
                             (s_to_string indent' sp2)
    | VExists (x, part) -> Printf.sprintf "%sVExists{%d}{%s}\n\n%s\n" indent (v_at (VExists (x, part)))
                             x (Part.to_string indent' (Var x) v_to_string part)
    | VForall (x, d, vp) -> Printf.sprintf "%sVForall{%d}{%s=%s}\n%s\n" indent (v_at p)
                              x (Dom.to_string d) (v_to_string indent' vp)
    | VPrev vp -> Printf.sprintf "%sVPrev{%d}\n%s" indent (v_at p) (v_to_string indent' vp)
    | VPrev0 -> Printf.sprintf "%sVPrev0{0}" indent
    | VPrevOutL i -> Printf.sprintf "%sVPrevOutL{%d}" indent i
    | VPrevOutR i -> Printf.sprintf "%sVPrevOutR{%d}" indent i
    | VNext vp -> Printf.sprintf "%sVNext{%d}\n%s" indent (v_at p) (v_to_string indent' vp)
    | VNextOutL i -> Printf.sprintf "%sVNextOutL{%d}" indent i
    | VNextOutR i -> Printf.sprintf "%sVNextOutR{%d}" indent i
    | VOnceOut i -> Printf.sprintf "%sVOnceOut{%d}" indent i
    | VOnce (_, etp, vps) -> Printf.sprintf "%sVOnce{%d}{%d}\n%s" indent (v_at p) etp
                               (Etc.deque_to_string indent' v_to_string vps)
    | VEventually (_, lrtp, vps) -> Printf.sprintf "%sVEventually{%d}{%d}\n%s" indent (v_at p) lrtp
                                     (Etc.deque_to_string indent' v_to_string vps)
    | VHistorically (_, vp) -> Printf.sprintf "%sVHistorically{%d}\n%s" indent (v_at p) (v_to_string indent' vp)
    | VAlways (_, vp) -> Printf.sprintf "%sVAlways{%d}\n%s" indent (v_at p) (v_to_string indent' vp)
    | VSinceOut i -> Printf.sprintf "%sVSinceOut{%d}" indent i
    | VSince (_, vp1, vp2s) -> Printf.sprintf "%sVSince{%d}\n%s\n%s" indent (v_at p) (v_to_string indent' vp1)
                                 (Etc.deque_to_string indent' v_to_string vp2s)
    | VSinceInf (_, etp, vp2s) -> Printf.sprintf "%sVSinceInf{%d}{%d}\n%s" indent (v_at p) etp
                                    (Etc.deque_to_string indent' v_to_string vp2s)
    | VUntil (_, vp1, vp2s) -> Printf.sprintf "%sVUntil{%d}\n%s\n%s" indent (v_at p)
                                 (Etc.deque_to_string indent' v_to_string vp2s) (v_to_string indent' vp1)
    | VUntilInf (_, lrtp, vp2s) -> Printf.sprintf "%sVUntilInf{%d}{%d}\n%s" indent (v_at p) lrtp
                                    (Etc.deque_to_string indent' v_to_string vp2s)

  let to_string indent = function
    | S sp -> s_to_string indent sp
    | V vp -> v_to_string indent vp

  let opt_to_string indent = function
    | None -> "none"
    | Some p -> to_string indent p

  let val_changes_to_latex v =
    if List.is_empty v then "v"
    else "v[" ^ Etc.string_list_to_string ~sep:", "
                  (List.map v ~f:(fun (x, d) -> Printf.sprintf "%s \\mapsto %s" x d)) ^ "]"

  let rec s_to_latex indent v idx p (h: Formula.t) =
    let indent' = "\t" ^ indent in
    match p, h with
    | STT tp, TT  ->
       Printf.sprintf "\\infer[\\top]{%s, %d \\pvd true}{}\n" (val_changes_to_latex v) tp
    | SEqConst (tp, x, c), EqConst (_, _) ->
       Printf.sprintf "\\infer[\\Seqconst]{%s, %d \\pvd %s = %s}{%s \\approx %s}\n"
         (val_changes_to_latex v) tp (Etc.escape_underscores x) (Dom.to_string c)
         (Etc.escape_underscores x) (Dom.to_string c)
    | SPred (tp, r, trms), Predicate (_, _) ->
       Printf.sprintf "\\infer[\\Spred]{%s, %d \\pvd %s\\,(%s)}{(%s,[%s]) \\in\\Gamma_{%d}}\n"
         (val_changes_to_latex v) tp (Etc.escape_underscores r) (Term.list_to_string trms)
         (Etc.escape_underscores r) (Term.list_to_string trms) tp
    | SNeg vp, Neg f ->
       Printf.sprintf "\\infer[\\Sneg]{%s, %d \\pvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent (v_to_latex indent' v idx vp f)
    | SOrL sp1, Or (f, g) ->
       Printf.sprintf "\\infer[\\Sdisjl]{%s, %d \\pvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent (s_to_latex indent' v idx sp1 f)
    | SOrR sp2, Or (f, g) ->
       Printf.sprintf "\\infer[\\Sdisjr]{%s, %d \\pvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent (s_to_latex indent' v idx sp2 g)
    | SAnd (sp1, sp2), And (f, g) ->
       Printf.sprintf "\\infer[\\Sconj]{%s, %d \\pvd %s}\n%s{{%s} & {%s}}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h)
         indent (s_to_latex indent' v idx sp1 f) (s_to_latex indent' v idx sp2 g)
    | SImpL vp1, Imp (f, g) ->
       Printf.sprintf "\\infer[\\SimpL]{%s, %d \\pvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent (v_to_latex indent' v idx vp1 f)
    | SImpR sp2, Imp (f, g) ->
       Printf.sprintf "\\infer[\\SimpR]{%s, %d \\pvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent (s_to_latex indent' v idx sp2 g)
    | SIffSS (sp1, sp2), Iff (f, g) ->
       Printf.sprintf "\\infer[\\SiffSS]{%s, %d \\pvd %s}\n%s{{%s} & {%s}}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h)
         indent (s_to_latex indent' v idx sp1 f) (s_to_latex indent' v idx sp2 g)
    | SIffVV (vp1, vp2), Iff (f, g) ->
       Printf.sprintf "\\infer[\\SiffVV]{%s, %d \\pvd %s}\n%s{{%s} & {%s}}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h)
         indent (v_to_latex indent' v idx vp1 f) (v_to_latex indent' v idx vp2 g)
    | SExists (x, d, sp), Exists (_, _, f) ->
       let v' = v @ [(x, Dom.to_string d)] in
       Printf.sprintf "\\infer[\\Sexists]{%s, %d \\pvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent (s_to_latex indent' v' idx sp f)
    | SForall (x, part), Forall (_, _, f) ->
       Printf.sprintf "\\infer[\\Sforall]{%s, %d \\pvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent
         (String.concat ~sep:"&" (List.map part ~f:(fun (sub, sp) ->
                                      let idx' = idx + 1 in
                                      let v' = v @ [(x, "d_" ^ (Int.to_string idx') ^ " \\in " ^ (Setc.to_latex sub))] in
                                      "{" ^ (s_to_latex indent' v' idx' sp f) ^ "}")))
    | SPrev sp, Prev (i, f) ->
       Printf.sprintf "\\infer[\\Sprev{}]{%s, %d \\pvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent (s_to_latex indent' v idx sp f)
    | SNext sp, Next (i, f) ->
       Printf.sprintf "\\infer[\\Snext{}]{%s, %d \\pvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent (s_to_latex indent' v idx sp f)
    | SOnce (tp, sp), Once (i, f) ->
       Printf.sprintf "\\infer[\\Sonce{}]{%s, %d \\pvd %s}\n%s{{%d \\leq %d} & {\\tau_%d - \\tau_%d \\in %s} & {%s}}\n"
         (val_changes_to_latex v) tp (Formula.to_latex h) indent
         (s_at sp) tp tp (s_at sp) (Interval.to_latex i) (s_to_latex indent' v idx sp f)
    | SEventually (tp, sp), Eventually (i, f) ->
       Printf.sprintf "\\infer[\\Seventually{}]{%s, %d \\pvd %s}\n%s{{%d \\geq %d} & {\\tau_%d - \\tau_%d \\in %s} & {%s}}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent
         (s_at sp) tp (s_at sp) tp (Interval.to_latex i) (s_to_latex indent' v idx sp f)
    | SHistorically (tp, _, sps), Historically (i, f) ->
       Printf.sprintf "\\infer[\\Shistorically{}]{%s, %d \\pvd %s}\n%s{{\\tau_%d - \\tau_0 \\geq %d} & %s}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent tp (Interval.left i)
         (String.concat ~sep:"&" (Fdeque.to_list (Fdeque.map sps ~f:(fun sp -> "{" ^ (s_to_latex indent' v idx sp f) ^ "}"))))
    | SHistoricallyOut _, Historically (i, f) ->
       Printf.sprintf "\\infer[\\ShistoricallyL{}]{%s, %d \\pvd %s}\n%s{\\tau_%d - \\tau_0 < %d}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent (s_at p) (Interval.left i)
    | SAlways (_, _, sps), Always (i, f) ->
       Printf.sprintf "\\infer[\\Salways{}]{%s, %d \\pvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent
         (String.concat ~sep:"&" (Fdeque.to_list (Fdeque.map sps ~f:(fun sp -> "{" ^ (s_to_latex indent' v idx sp f) ^ "}"))))
    | SSince (sp2, sp1s), Since (i, f, g) ->
       Printf.sprintf "\\infer[\\Ssince{}]{%s, %d \\pvd %s}\n%s{{%d \\leq %d} & {\\tau_%d - \\tau_%d \\in %s} & {%s} & %s}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent
         (s_at sp2) (s_at p) (s_at p) (s_at sp2) (Interval.to_latex i) (s_to_latex indent' v idx sp2 g)
         (String.concat ~sep:"&" (Fdeque.to_list (Fdeque.map sp1s ~f:(fun sp -> "{" ^ (s_to_latex indent' v idx sp f) ^ "}"))))
    | SUntil (sp2, sp1s), Until (i, f, g) ->
       Printf.sprintf "\\infer[\\Suntil{}]{%s, %d \\pvd %s}\n%s{{%d \\leq %d} & {\\tau_%d - \\tau_%d \\in %s} & %s & {%s}}\n"
         (val_changes_to_latex v) (s_at p) (Formula.to_latex h) indent
         (s_at p) (s_at sp2) (s_at sp2) (s_at p) (Interval.to_latex i)
         (String.concat ~sep:"&" (Fdeque.to_list (Fdeque.map sp1s ~f:(fun sp -> "{" ^ (s_to_latex indent' v idx sp f) ^ "}"))))
         (s_to_latex indent' v idx sp2 g)
    | _ -> ""
  and v_to_latex indent v idx p (h: Formula.t) =
    let indent' = "\t" ^ indent in
    match p, h with
    | VFF tp, FF ->
       Printf.sprintf "\\infer[\\bot]{%s, %d \\nvd false}{}\n" (val_changes_to_latex v) tp
    | VEqConst (tp, x, c), EqConst (_, _) ->
       Printf.sprintf "\\infer[\\Veqconst]{%s, %d \\nvd %s = %s}{%s \\not\\approx %s}\n"
         (val_changes_to_latex v) tp (Etc.escape_underscores x) (Dom.to_string c)
         (Etc.escape_underscores x) (Dom.to_string c)
    | VPred (tp, r, trms), Predicate (_, _) ->
       Printf.sprintf "\\infer[\\Vpred]{%s, %d \\nvd %s\\,(%s)}{(%s,[%s]) \\notin\\Gamma_{%d}}\n"
         (val_changes_to_latex v) tp (Etc.escape_underscores r) (Term.list_to_string trms)
         (Etc.escape_underscores r) (Term.list_to_string trms) tp
    | VNeg sp, Neg f ->
       Printf.sprintf "\\infer[\\Vneg]{%s, %d \\nvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent (s_to_latex indent' v idx sp f)
    | VOr (vp1, vp2), Or (f, g) ->
       Printf.sprintf "\\infer[\\Vdisj]{%s, %d \\nvd %s}\n%s{{%s} & {%s}}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h)
         indent (v_to_latex indent' v idx vp1 f) (v_to_latex indent' v idx vp2 g)
    | VAndL vp1, And (f, _) ->
       Printf.sprintf "\\infer[\\Vconjl]{%s, %d \\nvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent (v_to_latex indent' v idx vp1 f)
    | VAndR vp2, And (_, g) ->
       Printf.sprintf "\\infer[\\Vconjr]{%s, %d \\nvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent (v_to_latex indent' v idx vp2 g)
    | VImp (sp1, vp2), Imp (f, g) ->
       Printf.sprintf "\\infer[\\Vimp]{%s, %d \\nvd %s}\n%s{{%s} & {%s}}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h)
         indent (s_to_latex indent' v idx sp1 f) (v_to_latex indent' v idx vp2 g)
    | VIffSV (sp1, vp2), Iff (f, g) ->
       Printf.sprintf "\\infer[\\ViffSV]{%s, %d \\nvd %s}\n%s{{%s} & {%s}}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h)
         indent (s_to_latex indent' v idx sp1 f) (v_to_latex indent' v idx vp2 g)
    | VIffVS (vp1, sp2), Iff (f, g) ->
       Printf.sprintf "\\infer[\\ViffSV]{%s, %d \\nvd %s}\n%s{{%s} & {%s}}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h)
         indent (v_to_latex indent' v idx vp1 f) (s_to_latex indent' v idx sp2 g)
    | VExists (x, part), Exists (_, _, f) ->
       Printf.sprintf "\\infer[\\Vexists]{%s, %d \\nvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent
         (String.concat ~sep:"&" (List.map part ~f:(fun (sub, vp) ->
                                      let idx' = idx + 1 in
                                      let v' = v @ [(x, "d_" ^ (Int.to_string idx') ^ " \\in " ^ (Setc.to_latex sub))] in
                                      "{" ^ (v_to_latex indent' v' idx' vp f) ^ "}")))
    | VForall (x, d, vp), Forall (_, _, f) ->
       let v' = v @ [(x, Dom.to_string d)] in
       Printf.sprintf "\\infer[\\Vforall]{%s, %d \\nvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent (v_to_latex indent' v' idx vp f)
    | VPrev vp, Prev (i, f) ->
       Printf.sprintf "\\infer[\\Vprev{}]{%s, %d \\nvd %s}\n%s{{%d > 0} & {%s}}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent (v_at p) (v_to_latex indent' v idx vp f)
    | VPrev0, Prev (i, f) ->
       Printf.sprintf "\\infer[\\Vprevz]{%s, %d \\nvd %s}{}\n" (val_changes_to_latex v) (v_at p) (Formula.to_latex h)
    | VPrevOutL tp, Prev (i, f) ->
       Printf.sprintf "\\infer[\\Vprevl]{%s, %d \\nvd %s}\n%s{{%d > 0} & {\\tau_%d - \\tau_%d < %d}}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent (v_at p) (v_at p) ((v_at p)-1) (Interval.left i)
    | VPrevOutR tp, Prev (i, f) ->
       Printf.sprintf "\\infer[\\Vprevr]{%s, %d \\nvd %s}\n%s{{%d > 0} & {\\tau_%d - \\tau_%d > %d}}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent (v_at p) (v_at p) ((v_at p)-1)
         (Option.value_exn (Interval.right i))
    | VNext vp, Next (i, f) ->
       Printf.sprintf "\\infer[\\Vnext{}]{%s, %d \\nvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent (v_to_latex indent' v idx vp f)
    | VNextOutL tp, Next (i, f) ->
       Printf.sprintf "\\infer[\\Vnextl]{%s, %d \\nvd %s}{\\tau_%d - \\tau_%d < %d}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) ((v_at p)+1) (v_at p) (Interval.left i)
    | VNextOutR tp, Next (i, f) ->
       Printf.sprintf "\\infer[\\Vnextr]{%s, %d \\nvd %s}{\\tau_%d - \\tau_%d > %d}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) ((v_at p)+1) (v_at p) (Option.value_exn (Interval.right i))
    | VOnceOut tp, Once (i, f) ->
       Printf.sprintf "\\infer[\\Voncel{}]{%s, %d \\nvd %s}\n%s{\\tau_%d - \\tau_0 < %d}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent (v_at p) (Interval.left i)
    | VOnce (_, _, vps), Once (i, f) ->
       Printf.sprintf "\\infer[\\Vonce{}]{%s, %d \\nvd %s}\n%s{{\\tau_%d - \\tau_0 \\geq %d} & %s}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent (v_at p) (Interval.left i)
         (String.concat ~sep:"&" (Fdeque.to_list (Fdeque.map vps ~f:(fun vp -> "{" ^ (v_to_latex indent' v idx vp f) ^ "}"))))
    | VEventually (_, _, vps), Eventually (i, f) ->
       Printf.sprintf "\\infer[\\Veventually{}]{%s, %d \\nvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent
         (String.concat ~sep:"&" (Fdeque.to_list (Fdeque.map vps ~f:(fun vp -> "{" ^ (v_to_latex indent' v idx vp f) ^ "}"))))
    | VHistorically (tp, vp), Historically (i, f) ->
       Printf.sprintf "\\infer[\\Vhistorically{}]{%s, %d \\nvd %s}\n%s{{%d \\leq %d} & {\\tau_%d - \\tau_%d \\in %s} & {%s}}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent
         (v_at vp) tp tp (v_at vp) (Interval.to_latex i) (v_to_latex indent' v idx vp f)
    | VAlways (tp, vp), Always (i, f) ->
       Printf.sprintf "\\infer[\\Valways{}]{%s, %d \\nvd %s}\n%s{{%d \\geq %d} & {\\tau_%d - \\tau_%d \\in %s} & {%s}}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent
         (v_at vp) tp (v_at vp) tp (Interval.to_latex i) (v_to_latex indent' v idx vp f)
    | VSinceOut tp, Since (i, f, g) ->
       Printf.sprintf "\\infer[\\Vsincel{}]{%s, %d \\nvd %s}\n%s{\\tau_%d - \\tau_0 < %d}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent (v_at p) (Interval.left i)
    | VSince (tp, vp1, vp2s), Since (i, f, g) ->
       Printf.sprintf "\\infer[\\Vsince{}]{%s, %d \\nvd %s}\n%s{{%d \\leq %d} & {\\tau_%d - \\tau_0 \\geq %d} & {%s} & %s}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent
         (v_at vp1) tp tp (Interval.left i) (v_to_latex indent' v idx vp1 f)
         (String.concat ~sep:"&" (Fdeque.to_list (Fdeque.map vp2s ~f:(fun vp -> "{" ^ (v_to_latex indent' v idx vp g) ^ "}"))))
    | VSinceInf (tp, _, vp2s), Since (i, f, g) ->
       Printf.sprintf "\\infer[\\Vsinceinf{}]{%s, %d \\nvd %s}\n%s{{\\tau_%d - \\tau_0 \\geq %d} & %s}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent tp (Interval.left i)
         (String.concat ~sep:"&" (Fdeque.to_list (Fdeque.map vp2s ~f:(fun vp -> "{" ^ (v_to_latex indent' v idx vp g) ^ "}"))))
    | VUntil (tp, vp1, vp2s), Until (i, f, g) ->
       Printf.sprintf "\\infer[\\Vuntil{}]{%s, %d \\nvd %s}\n%s{{%d \\leq %d} & %s & {%s}}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent tp (v_at vp1)
         (String.concat ~sep:"&" (Fdeque.to_list (Fdeque.map vp2s ~f:(fun vp -> "{" ^ (v_to_latex indent' v idx vp g) ^ "}"))))
         (v_to_latex indent' v idx vp1 f)
    | VUntilInf (_, _, vp2s), Until (i, f, g) ->
       Printf.sprintf "\\infer[\\Vuntil{}]{%s, %d \\nvd %s}\n%s{%s}\n"
         (val_changes_to_latex v) (v_at p) (Formula.to_latex h) indent
         (String.concat ~sep:"&" (Fdeque.to_list (Fdeque.map vp2s ~f:(fun vp -> "{" ^ (v_to_latex indent' v idx vp g) ^ "}"))))
    | _ -> ""

  let to_latex indent fmla = function
    | S sp -> s_to_latex indent [] 0 sp fmla
    | V vp -> v_to_latex indent [] 0 vp fmla

  let opt_to_latex indent fmla = function
    | None -> "None"
    | Some p -> to_latex indent fmla p

  let to_light indent = function
    | S _ -> "true\n"
    | V vp -> v_to_string indent vp

  let opt_to_light indent = function
    | None -> "none"
    | Some p -> to_light indent p

  let rec s_ertp = function
    | STT tp -> tp
    | SEqConst (tp, _, _)
      | SPred (tp, _, _) -> tp
    | SNeg vp -> v_ertp vp
    | SOrL sp1 -> s_ertp sp1
    | SOrR sp2 -> s_ertp sp2
    | SAnd (sp1, sp2) -> min (s_ertp sp1) (s_ertp sp2)
    | SImpL vp1 -> v_ertp vp1
    | SImpR sp2 -> s_ertp sp2
    | SIffSS (sp1, sp2) -> min (s_ertp sp1) (s_ertp sp2)
    | SIffVV (vp1, vp2) -> min (v_ertp vp1) (v_ertp vp2)
    | SExists (_, _, sp) -> s_ertp sp
    | SForall (_, part) -> Part.fold_left part max_int (fun acc sp -> min acc (s_ertp sp))
    | SPrev sp -> s_ertp sp
    | SNext sp -> s_ertp sp - 1
    | SOnce (_, sp) -> s_ertp sp
    | SEventually (tp, sp) -> min tp (s_ertp sp)
    | SHistoricallyOut tp -> tp
    | SHistorically (tp, _, sps)
      | SAlways (tp, _, sps) ->
       let min_sps = Fdeque.fold sps ~init:max_int
                       ~f:(fun acc sp -> min acc (s_ertp sp)) in
       min tp min_sps
    | SSince (sp2, sp1s)
      | SUntil (sp2, sp1s) ->
       let min_sp1s = Fdeque.fold sp1s ~init:max_int
                        ~f:(fun acc sp1 -> min acc (s_ertp sp1)) in
       min min_sp1s (s_ertp sp2)
  and v_ertp = function
    | VFF tp -> tp
    | VEqConst (tp, _, _)
      | VPred (tp, _, _) -> tp
    | VNeg sp -> s_ertp sp
    | VOr (vp1, vp2) -> min (v_ertp vp1) (v_ertp vp2)
    | VAndL vp1 -> v_ertp vp1
    | VAndR vp2 -> v_ertp vp2
    | VImp (sp1, vp2)
      | VIffSV (sp1, vp2) -> min (s_ertp sp1) (v_ertp vp2)
    | VIffVS (vp1, sp2) -> min (v_ertp vp1) (s_ertp sp2)
    | VExists (_, part) -> Part.fold_left part max_int (fun a vp -> min a (v_ertp vp))
    | VForall (_, _, vp) -> v_ertp vp
    | VPrev vp -> v_ertp vp
    | VPrev0 -> 0
    | VPrevOutL tp
      | VPrevOutR tp
      | VOnceOut tp -> tp
    | VNextOutL tp
      | VNextOutR tp -> tp
    | VNext vp -> v_ertp vp - 1
    | VOnce (tp, _, vps)
      | VEventually (tp, _, vps) ->
       let min_vps = Fdeque.fold vps ~init:max_int
                       ~f:(fun acc vp -> min acc (v_ertp vp)) in
       min tp min_vps
    | VHistorically (tp, vp)
      | VAlways (tp, vp) -> min tp (v_ertp vp)
    | VSinceOut tp -> tp
    | VSince (tp, vp1, vp2s)
      | VUntil (tp, vp1, vp2s) ->
       let min_vp2s = Fdeque.fold vp2s ~init:max_int
                        ~f:(fun acc vp2 -> min acc (v_ertp vp2)) in
       min tp (min min_vp2s (v_ertp vp1))
    | VSinceInf (tp, _, vp2s)
      | VUntilInf (tp, _, vp2s) ->
       let min_vp2s = Fdeque.fold vp2s ~init:max_int
                        ~f:(fun acc vp2 -> min acc (v_ertp vp2)) in
       min tp min_vp2s

  let ertp = function
    | S sp -> s_ertp sp
    | V vp -> v_ertp vp

  let rec s_lrtp = function
    | STT tp -> tp
    | SEqConst (tp, _, _)
      | SPred (tp, _, _) -> tp
    | SNeg vp -> v_lrtp vp
    | SOrL sp1 -> s_lrtp sp1
    | SOrR sp2 -> s_lrtp sp2
    | SAnd (sp1, sp2) -> max (s_lrtp sp1) (s_lrtp sp2)
    | SImpL vp1 -> v_lrtp vp1
    | SImpR sp2 -> s_lrtp sp2
    | SIffSS (sp1, sp2) -> max (s_lrtp sp1) (s_lrtp sp2)
    | SIffVV (vp1, vp2) -> max (v_lrtp vp1) (v_lrtp vp2)
    | SExists (_, _, sp) -> s_lrtp sp
    | SForall (_, part) -> Part.fold_left part 0 (fun acc sp -> max acc (s_lrtp sp))
    | SPrev sp -> s_lrtp sp + 1
    | SNext sp -> s_lrtp sp
    | SOnce (_, sp) -> s_lrtp sp
    | SEventually (tp, sp) -> max tp (s_lrtp sp)
    | SHistoricallyOut tp -> tp
    | SHistorically (tp, _, sps)
      | SAlways (tp, _, sps) ->
       let max_sps = Fdeque.fold sps ~init:0
                       ~f:(fun acc sp -> max acc (s_lrtp sp)) in
       max tp max_sps
    | SSince (sp2, sp1s)
      | SUntil (sp2, sp1s) ->
       let max_sp1s = Fdeque.fold sp1s ~init:0
                        ~f:(fun acc sp1 -> max acc (s_lrtp sp1)) in
       max max_sp1s (s_lrtp sp2)
  and v_lrtp = function
    | VFF tp -> tp
    | VEqConst (tp, _, _)
      | VPred (tp, _, _) -> tp
    | VNeg sp -> s_lrtp sp
    | VOr (vp1, vp2) -> max (v_lrtp vp1) (v_lrtp vp2)
    | VAndL vp1 -> v_lrtp vp1
    | VAndR vp2 -> v_lrtp vp2
    | VImp (sp1, vp2)
      | VIffSV (sp1, vp2) -> max (s_lrtp sp1) (v_lrtp vp2)
    | VIffVS (vp1, sp2) -> max (v_lrtp vp1) (s_lrtp sp2)
    | VExists (_, part) -> Part.fold_left part 0 (fun a vp -> max a (v_lrtp vp))
    | VForall (_, _, vp) -> v_lrtp vp
    | VPrev vp -> v_lrtp vp + 1
    | VPrev0 -> 0
    | VPrevOutL tp
      | VPrevOutR tp
      | VOnceOut tp -> tp
    | VNextOutL tp
      | VNextOutR tp -> tp
    | VNext vp -> v_lrtp vp
    | VOnce (tp, _, vps)
      | VEventually (tp, _, vps) ->
       let max_vps = Fdeque.fold vps ~init:0
                       ~f:(fun acc vp -> max acc (v_lrtp vp)) in
       max tp max_vps
    | VHistorically (tp, vp)
      | VAlways (tp, vp) -> max tp (v_lrtp vp)
    | VSinceOut tp -> tp
    | VSince (tp, vp1, vp2s)
      | VUntil (tp, vp1, vp2s) ->
       let max_vp2s = Fdeque.fold vp2s ~init:0
                        ~f:(fun acc vp2 -> max acc (v_lrtp vp2)) in
       max tp (max max_vp2s (v_lrtp vp1))
    | VSinceInf (tp, _, vp2s)
      | VUntilInf (tp, _, vp2s) ->
       let max_vp2s = Fdeque.fold vp2s ~init:0
                        ~f:(fun acc vp2 -> max acc (v_lrtp vp2)) in
       max tp max_vp2s

  let lrtp = function
    | S sp -> s_lrtp sp
    | V vp -> v_lrtp vp

  module Size = struct

    let sum f d = Fdeque.fold d ~init:0 ~f:(fun acc p -> acc + f p)

    let rec s = function
      | STT _ -> 1
      | SEqConst _ -> 1
      | SPred _ -> 1
      | SNeg vp -> 1 + v vp
      | SOrL sp1 -> 1 + s sp1
      | SOrR sp2 -> 1 + s sp2
      | SAnd (sp1, sp2) -> 1 + s sp1 + s sp2
      | SImpL vp1 -> 1 + v vp1
      | SImpR sp2 -> 1 + s sp2
      | SIffSS (sp1, sp2) -> 1 + s sp1 + s sp2
      | SIffVV (vp1, vp2) -> 1 + v vp1 + v vp2
      | SExists (_, _, sp) -> 1 + s sp
      | SForall (_, part) -> 1 + (Part.fold_left part 0 (fun a sp -> a + s sp))
      | SPrev sp -> 1 + s sp
      | SNext sp -> 1 + s sp
      | SOnce (_, sp) -> 1 + s sp
      | SEventually (_, sp) -> 1 + s sp
      | SHistorically (_, _, sps) -> 1 + sum s sps
      | SHistoricallyOut _ -> 1
      | SAlways (_, _, sps) -> 1 + sum s sps
      | SSince (sp2, sp1s) -> 1 + s sp2 + sum s sp1s
      | SUntil (sp2, sp1s) -> 1 + s sp2 + sum s sp1s
    and v = function
      | VFF _ -> 1
      | VEqConst _ -> 1
      | VPred _ -> 1
      | VNeg sp -> 1 + s sp
      | VOr (vp1, vp2) -> 1 + v vp1 + v vp2
      | VAndL vp1 -> 1 + v vp1
      | VAndR vp2 -> 1 + v vp2
      | VImp (sp1, vp2) -> 1 + s sp1 + v vp2
      | VIffSV (sp1, vp2) -> 1 + s sp1 + v vp2
      | VIffVS (vp1, sp2) -> 1 + v vp1 + s sp2
      | VExists (_, part) -> 1 + (Part.fold_left part 0 (fun a vp -> a + v vp))
      | VForall (_, _, vp) -> 1 + v vp
      | VPrev vp -> 1 + v vp
      | VPrev0 -> 1
      | VPrevOutL _ -> 1
      | VPrevOutR _ -> 1
      | VNext vp -> 1 + v vp
      | VNextOutL _ -> 1
      | VNextOutR _ -> 1
      | VOnceOut _ -> 1
      | VOnce (_, _, vp1s) -> 1 + sum v vp1s
      | VEventually (_, _, vp1s) -> 1 + sum v vp1s
      | VHistorically (_, vp1) -> 1 + v vp1
      | VAlways (_, vp1) -> 1 + v vp1
      | VSinceOut _ -> 1
      | VSince (_, vp1, vp2s) -> 1 + v vp1 + sum v vp2s
      | VSinceInf (_, _, vp2s) -> 1 + sum v vp2s
      | VUntil (_, vp1, vp2s) -> 1 + v vp1 + sum v vp2s
      | VUntilInf (_, _, vp2s) -> 1 + sum v vp2s

    let p = function
      | S s_p -> s s_p
      | V v_p -> v v_p

    let minp_bool = cmp p

    let minp x y = if p x <= p y then x else y

    let minp_list = function
      | [] -> None
      | x :: xs -> Some (List.fold_left xs ~init:x ~f:minp)

    let minp_list_somes ps =
      minp_list (List.map ps ~f:(fun p -> Option.value_exn p))

  end

end

module Pdt = struct

  type 'a t = Leaf of 'a | Node of string * ('a t) Part.t

  let rec apply1 vars f pdt = match vars, pdt with
    | _ , Leaf l -> Leaf (f l)
    | z :: vars, Node (x, part) ->
       if String.equal x z then
         Node (x, Part.map part (apply1 vars f))
       else apply1 vars f (Node (x, part))
    | _ -> raise (Invalid_argument "variable list is empty")

  let rec apply2 vars f pdt1 pdt2 = match vars, pdt1, pdt2 with
    | _ , Leaf l1, Leaf l2 -> Leaf (f l1 l2)
    | _ , Leaf l1, Node (x, part2) -> Node (x, Part.map part2 (apply1 vars (f l1)))
    | _ , Node (x, part1), Leaf l2 -> Node (x, Part.map part1 (apply1 vars (fun l1 -> f l1 l2)))
    | z :: vars, Node (x, part1), Node (y, part2) ->
       if String.equal x z && String.equal y z then
         Node (z, Part.merge2 (apply2 vars f) part1 part2)
       else (if String.equal x z then
               Node (x, Part.map part1 (fun pdt1 -> apply2 vars f pdt1 (Node (y, part2))))
             else (if String.equal y z then
                     Node (y, Part.map part2 (apply2 vars f (Node (x, part1))))
                   else apply2 vars f (Node (x, part1)) (Node (y, part2))))
    | _ -> raise (Invalid_argument "variable list is empty")

  let rec apply3 vars f pdt1 pdt2 pdt3 = match vars, pdt1, pdt2, pdt3 with
    | _ , Leaf l1, Leaf l2, Leaf l3 -> Leaf (f l1 l2 l3)
    | _ , Leaf l1, Leaf l2, Node (x, part3) ->
       Node (x, Part.map part3 (apply1 vars (fun l3 -> f l1 l2 l3)))
    | _ , Leaf l1, Node (x, part2), Leaf l3 ->
       Node (x, Part.map part2 (apply1 vars (fun l2 -> f l1 l2 l3)))
    | _ , Node (x, part1), Leaf l2, Leaf l3 ->
       Node (x, Part.map part1 (apply1 vars (fun l1 -> f l1 l2 l3)))
    | w :: vars, Leaf l1, Node (y, part2), Node (z, part3) ->
       if String.equal y w && String.equal z w then
         Node (w, Part.merge2 (apply2 vars (f l1)) part2 part3)
       else (if String.equal y w then
               Node (y, Part.map part2 (fun pdt2 -> apply2 vars (f l1) pdt2 (Node (z, part3))))
             else (if String.equal z w then
                     Node (z, Part.map part3 (fun pdt3 -> apply2 vars (f l1) (Node (y, part2)) pdt3))
                   else apply3 vars f (Leaf l1) (Node (y, part2)) (Node(z, part3))))
    | w :: vars, Node (x, part1), Node (y, part2), Leaf l3 ->
       if String.equal x w && String.equal y w then
         Node (w, Part.merge2 (apply2 vars (fun l1 l2 -> f l1 l2 l3)) part1 part2)
       else (if String.equal x w then
               Node (x, Part.map part1 (fun pdt1 -> apply2 vars (fun pt1 pt2 -> f pt1 pt2 l3) pdt1 (Node (y, part2))))
             else (if String.equal y w then
                     Node (y, Part.map part2 (fun pdt2 -> apply2 vars (fun l1 l2 -> f l1 l2 l3) (Node (x, part1)) pdt2))
                   else apply3 vars f (Node (x, part1)) (Node (y, part2)) (Leaf l3)))
    | w :: vars, Node (x, part1), Leaf l2, Node (z, part3) ->
       if String.equal x w && String.equal z w then
         Node (w, Part.merge2 (apply2 vars (fun l1 -> f l1 l2)) part1 part3)
       else (if String.equal x w then
               Node (x, Part.map part1 (fun pdt1 -> apply2 vars (fun l1 -> f l1 l2) pdt1 (Node (z, part3))))
             else (if String.equal z w then
                     Node (z, Part.map part3 (fun pdt3 -> apply2 vars (fun l1 -> f l1 l2) (Node (x, part1)) pdt3))
                   else apply3 vars f (Node (x, part1)) (Leaf l2) (Node (z, part3))))
    | w :: vars, Node (x, part1), Node (y, part2), Node (z, part3) ->
       if String.equal x w && String.equal y w && String.equal z w then
         Node (z, Part.merge3 (apply3 vars f) part1 part2 part3)
       else (if String.equal x w && String.equal y w then
               Node (w, Part.merge2 (fun pdt1 pdt2 -> apply3 vars f pdt1 pdt2 (Node (z, part3))) part1 part2)
             else (if String.equal x w && String.equal z w then
                     Node (w, Part.merge2 (fun pdt1 pdt3 -> apply3 vars f pdt1 (Node (y, part2)) pdt3) part1 part3)
                   else (if String.equal y w && String.equal z w then
                           Node (w, Part.merge2 (apply3 vars (fun l1 -> f l1) (Node (x, part1))) part2 part3)
                         else (if String.equal x w then
                                 Node (x, Part.map part1 (fun pdt1 -> apply3 vars f pdt1 (Node (y, part2)) (Node (z, part3))))
                               else (if String.equal y w then
                                       Node (y, Part.map part2 (fun pdt2 -> apply3 vars f (Node (x, part1)) pdt2
                                                                              (Node (z, part3))))
                                     else (if String.equal z w then
                                             Node (z, Part.map part3 (fun pdt3 -> apply3 vars f (Node (x, part1))
                                                                                    (Node (y, part2)) pdt3))
                                           else apply3 vars f (Node (x, part1)) (Node (y, part2)) (Node (z, part3))))))))
    | _ -> raise (Invalid_argument "variable list is empty")

  let rec split_prod = function
    | Leaf (l1, l2) -> (Leaf l1, Leaf l2)
    | Node (x, part) -> let (part1, part2) = Part.split_prod (Part.map part split_prod) in
                        (Node (x, part1), Node (x, part2))

  let rec split_list = function
    | Leaf l -> List.map l ~f:(fun el -> Leaf el)
    | Node (x, part) -> List.map (Part.split_list (Part.map part split_list)) ~f:(fun el -> Node (x, el))

  let is_leaf = function
    | Leaf _ -> true
    | Node _ -> false

  let is_node = function
    | Leaf _ -> false
    | Node _ -> true

  let rec to_string f indent pdt =
    let indent' = (String.make 4 ' ') ^ indent in
    match pdt with
    | Leaf pt -> Printf.sprintf "%s❮\n\n%s\n%s❯" indent' ((f indent') pt) indent'
    | Node (x, part) -> (Part.to_string indent' (Var x) (to_string f) part)

  let rec to_latex f indent = function
    | Leaf pt -> Printf.sprintf "%s%s\n" indent (f pt)
    | Node (x, part) -> (Part.to_string indent (Var x) (to_latex f) part)

  let rec to_light_string f indent = function
    | Leaf pt -> Printf.sprintf "%s%s\n" indent (f pt)
    | Node (x, part) -> (Part.to_string indent (Var x) (to_light_string f) part)

  let unleaf = function
    | Leaf l -> l
    | _ -> raise (Invalid_argument "function not defined for nodes")

  let rec hide vars f_leaf f_node pdt = match vars, pdt with
    |  _ , Leaf l -> Leaf (f_leaf l)
    | [x], Node (y, part) -> Leaf (f_node (Part.map part unleaf))
    | x :: vars, Node (y, part) -> if String.equal x y then
                                     Node (y, Part.map part (hide vars f_leaf f_node))
                                   else hide vars f_leaf f_node (Node (y, part))
    | _ -> raise (Invalid_argument "function not defined for other cases")

  (* reduce related *)
  let rec equal p_eq pdt1 pdt2 =
    match pdt1, pdt2 with
    | Leaf l1, Leaf l2 -> p_eq l1 l2
    | Node (x, part), Node (x', part') -> String.equal x x' && Int.equal (Part.length part) (Part.length part') &&
                                            List.for_all2_exn part part' ~f:(fun (s, v) (s', v') ->
                                                Setc.equal s s' && equal p_eq v v')

  let rec reduce p_eq = function
    | Leaf l -> Leaf l
    | Node (x, part) -> Node (x, Part.dedup (equal p_eq) (Part.map part (reduce p_eq)))

  let rec apply1_reduce p_eq vars f pdt = match vars, pdt with
    | _ , Leaf l -> Leaf (f l)
    | z :: vars, Node (x, part) ->
       if String.equal x z then
         Node (x, Part.map_dedup (equal p_eq) part (apply1_reduce p_eq vars f))
       else apply1_reduce p_eq vars f (Node (x, part))
    | _ -> raise (Invalid_argument "variable list is empty")

  let rec apply2_reduce p_eq vars f pdt1 pdt2 = match vars, pdt1, pdt2 with
    | _ , Leaf l1, Leaf l2 -> Leaf (f l1 l2)
    | _ , Leaf l1, Node (x, part2) -> Node (x, Part.map_dedup (equal p_eq) part2 (apply1_reduce p_eq vars (f l1)))
    | _ , Node (x, part1), Leaf l2 -> Node (x, Part.map_dedup (equal p_eq) part1
                                                 (apply1_reduce p_eq vars (fun l1 -> f l1 l2)))
    | z :: vars, Node (x, part1), Node (y, part2) ->
       if String.equal x z && String.equal y z then
         Node (z, Part.merge2_dedup (equal p_eq) (apply2_reduce p_eq vars f) part1 part2)
       else (if String.equal x z then
               Node (x, Part.map_dedup (equal p_eq) part1 (fun pdt1 -> apply2_reduce p_eq vars f pdt1 (Node (y, part2))))
             else (if String.equal y z then
                     Node (y, Part.map_dedup (equal p_eq) part2 (apply2_reduce p_eq vars f (Node (x, part1))))
                   else apply2_reduce p_eq vars f (Node (x, part1)) (Node (y, part2))))
    | _ -> raise (Invalid_argument "variable list is empty")

  let rec apply3_reduce p_eq vars f pdt1 pdt2 pdt3 = match vars, pdt1, pdt2, pdt3 with
    | _ , Leaf l1, Leaf l2, Leaf l3 -> Leaf (f l1 l2 l3)
    | _ , Leaf l1, Leaf l2, Node (x, part3) ->
       Node (x, Part.map_dedup (equal p_eq) part3 (apply1_reduce p_eq vars (fun l3 -> f l1 l2 l3)))
    | _ , Leaf l1, Node (x, part2), Leaf l3 ->
       Node (x, Part.map_dedup (equal p_eq) part2 (apply1_reduce p_eq vars (fun l2 -> f l1 l2 l3)))
    | _ , Node (x, part1), Leaf l2, Leaf l3 ->
       Node (x, Part.map_dedup (equal p_eq) part1 (apply1_reduce p_eq vars (fun l1 -> f l1 l2 l3)))
    | w :: vars, Leaf l1, Node (y, part2), Node (z, part3) ->
       if String.equal y w && String.equal z w then
         Node (w, Part.merge2_dedup (equal p_eq) (apply2_reduce p_eq vars (f l1)) part2 part3)
       else (if String.equal y w then
               Node (y, Part.map_dedup (equal p_eq) part2
                          (fun pdt2 -> apply2_reduce p_eq vars (f l1) pdt2 (Node (z, part3))))
             else (if String.equal z w then
                     Node (z, Part.map_dedup (equal p_eq) part3
                                (fun pdt3 -> apply2_reduce p_eq vars (f l1) (Node (y, part2)) pdt3))
                   else apply3_reduce p_eq vars f (Leaf l1) (Node (y, part2)) (Node(z, part3))))
    | w :: vars, Node (x, part1), Node (y, part2), Leaf l3 ->
       if String.equal x w && String.equal y w then
         Node (w, Part.merge2_dedup (equal p_eq) (apply2_reduce p_eq vars (fun l1 l2 -> f l1 l2 l3)) part1 part2)
       else (if String.equal x w then
               Node (x, Part.map_dedup (equal p_eq) part1
                          (fun pdt1 -> apply2_reduce p_eq vars (fun pt1 pt2 -> f pt1 pt2 l3) pdt1 (Node (y, part2))))
             else (if String.equal y w then
                     Node (y, Part.map_dedup (equal p_eq) part2
                                (fun pdt2 -> apply2_reduce p_eq vars (fun l1 l2 -> f l1 l2 l3) (Node (x, part1)) pdt2))
                   else apply3_reduce p_eq vars f (Node (x, part1)) (Node (y, part2)) (Leaf l3)))
    | w :: vars, Node (x, part1), Leaf l2, Node (z, part3) ->
       if String.equal x w && String.equal z w then
         Node (w, Part.merge2_dedup (equal p_eq) (apply2_reduce p_eq vars (fun l1 -> f l1 l2)) part1 part3)
       else (if String.equal x w then
               Node (x, Part.map_dedup (equal p_eq) part1
                          (fun pdt1 -> apply2_reduce p_eq vars (fun l1 -> f l1 l2) pdt1 (Node (z, part3))))
             else (if String.equal z w then
                     Node (z, Part.map_dedup (equal p_eq) part3
                                (fun pdt3 -> apply2_reduce p_eq vars (fun l1 -> f l1 l2) (Node (x, part1)) pdt3))
                   else apply3_reduce p_eq vars f (Node (x, part1)) (Leaf l2) (Node (z, part3))))
    | w :: vars, Node (x, part1), Node (y, part2), Node (z, part3) ->
       if String.equal x w && String.equal y w && String.equal z w then
         Node (z, Part.merge3_dedup (equal p_eq) (apply3_reduce p_eq vars f) part1 part2 part3)
       else (if String.equal x w && String.equal y w then
               Node (w, Part.merge2_dedup (equal p_eq) (fun pdt1 pdt2 ->
                            apply3_reduce p_eq vars f pdt1 pdt2 (Node (z, part3))) part1 part2)
             else (if String.equal x w && String.equal z w then
                     Node (w, Part.merge2_dedup (equal p_eq) (fun pdt1 pdt3 ->
                                  apply3_reduce p_eq vars f pdt1 (Node (y, part2)) pdt3) part1 part3)
                   else (if String.equal y w && String.equal z w then
                           Node (w, Part.merge2_dedup (equal p_eq)
                                      (apply3_reduce p_eq vars (fun l1 -> f l1) (Node (x, part1))) part2 part3)
                         else (if String.equal x w then
                                 Node (x, Part.map_dedup (equal p_eq) part1 (fun pdt1 ->
                                              apply3_reduce p_eq vars f pdt1 (Node (y, part2)) (Node (z, part3))))
                               else (if String.equal y w then
                                       Node (y, Part.map_dedup (equal p_eq) part2 (fun pdt2 ->
                                                    apply3_reduce p_eq vars f (Node (x, part1)) pdt2 (Node (z, part3))))
                                     else (if String.equal z w then
                                             Node (z, Part.map_dedup (equal p_eq) part3 (fun pdt3 ->
                                                          apply3_reduce p_eq vars f (Node (x, part1))
                                                            (Node (y, part2)) pdt3))
                                           else apply3_reduce p_eq vars f (Node (x, part1))
                                                  (Node (y, part2)) (Node (z, part3))))))))
    | _ -> raise (Invalid_argument "variable list is empty")

  let rec split_prod_reduce p_eq = function
    | Leaf (l1, l2) -> (Leaf l1, Leaf l2)
    | Node (x, part) -> let (part1, part2) = Part.split_prod_dedup (equal p_eq)
                                               (Part.map part (split_prod_reduce p_eq)) in
                        (Node (x, part1), Node (x, part2))

  let rec split_list_reduce p_eq = function
    | Leaf l -> List.map l ~f:(fun el -> Leaf el)
    | Node (x, part) -> List.map (Part.split_list_dedup (equal p_eq) (Part.map part (split_list_reduce p_eq)))
                          ~f:(fun el -> Node (x, el))

  let rec hide_reduce p_eq vars f_leaf f_node pdt = match vars, pdt with
    |  _ , Leaf l -> Leaf (f_leaf l)
    | [x], Node (y, part) -> Leaf (f_node (Part.map part unleaf))
    | x :: vars, Node (y, part) -> if String.equal x y then
                                     Node (y, Part.map_dedup (equal p_eq) part (hide_reduce p_eq vars f_leaf f_node))
                                   else hide_reduce p_eq vars f_leaf f_node (Node (y, part))
    | _ -> raise (Invalid_argument "function not defined for other cases")

  let rec somes = function
    | Leaf l -> Leaf (Some l)
    | Node (x, part) -> Node (x, Part.map part (fun expl -> somes expl))

  let rec unsomes = function
    | Leaf None -> raise (Invalid_argument "found nones in the PDT")
    | Leaf (Some l) -> Leaf l
    | Node (x, part) -> Node (x, Part.map part (fun expl -> unsomes expl))

  let rec somes_pol pol = function
    | Leaf l -> (match l, pol with
                 | Proof.S _, Polarity.SAT
                   | V _, VIO -> Leaf (Some l)
                 | _ -> Leaf None)
    | Node (x, part) -> Node (x, Part.map part (fun expl -> somes expl))

  let rec uneither = function
    | Leaf (Either.First l) -> Leaf l
    | Leaf (Either.Second _) -> Leaf None
    | Node (x, part) -> Node (x, Part.map part (fun expl -> uneither expl))

  let rec prune_nones = function
    | Leaf l_opt -> (match l_opt with
                     | None -> None
                     | Some l -> Some (Leaf l))
    | Node (x, part) -> Some (Node (x, Part.filter_map part (fun (s, pdt_opt) -> match prune_nones pdt_opt with
                                                                                 | None -> None
                                                                                 | Some pdt -> Some (s, pdt))))

end

type t = Proof.t Pdt.t
type opt_t = Proof.t option Pdt.t

let rec equal expl expl' = match expl, expl' with
  | Pdt.Leaf l, Pdt.Leaf l' -> Proof.equal l l'
  | Node (x, part), Node (x', part') -> String.equal x x' && Part.equal part part' equal
  | _ -> false

let rec opt_equal expl expl' = match expl, expl' with
  | Pdt.Leaf l, Pdt.Leaf l' -> Proof.opt_equal l l'
  | Node (x, part), Node (x', part') -> String.equal x x' && Part.equal part part' opt_equal
  | _ -> false

let rec exists_violation = function
  | Pdt.Leaf l -> (match l with
                   | Proof.V _ -> true
                   | Proof.S _ -> false)
  | Node (x, part) -> Part.exists part exists_violation

let rec opt_exists_violation = function
  | Pdt.Leaf l -> (match l with
                   | Some (Proof.V _) -> true
                   | _ -> false)
  | Node (x, part) -> Part.exists part opt_exists_violation

let rec opt_exists_satisfaction = function
  | Pdt.Leaf l -> (match l with
                   | Some (Proof.S _) -> true
                   | _ -> false)
  | Node (x, part) -> Part.exists part opt_exists_satisfaction

let rec opt_all_none = function
  | Pdt.Leaf l -> (match l with
                   | None -> true
                   | Some _ -> false)
  | Node (x, part) -> Part.for_all part opt_all_none

let at expl =
  let rec at_rec = function
    | Pdt.Leaf pt -> Proof.p_at pt
    | Node (_, part) -> at_rec (Part.hd part) in
  at_rec  expl

let opt_at expl =
  let rec opt_at_rec = function
    | Pdt.Leaf pt -> Proof.opt_p_at pt
    | Node (_, part) -> Part.find_map part (fun expl -> opt_at_rec expl) in
  Option.value_exn (opt_at_rec  expl)

let rec sort_parts = function
  | Pdt.Leaf pt -> Pdt.Leaf pt
  | Node (x, part) -> Node (x, Part.map (Part.sort part) sort_parts)

let rec ertp = function
  | Pdt.Leaf pt -> Proof.ertp pt
  | Node (_, part) -> Part.fold_left part max_int (fun a expl -> min a (ertp expl))

let rec lrtp = function
  | Pdt.Leaf pt -> Proof.lrtp pt
  | Node (_, part) -> Part.fold_left part 0 (fun a expl -> max a (lrtp expl))

let to_string expl = Pdt.to_string Proof.to_string "" expl

let opt_to_string expl = Pdt.to_string Proof.opt_to_string "" expl

let to_latex fmla expl = Pdt.to_latex (Proof.to_latex "" fmla) "" expl

let to_light_string expl = Pdt.to_light_string (Proof.to_light "") "" expl

(* GUI related (the function below DO NOT produce PDTs).            *)
(* In particular, the subsets at each level DO NOT form partitions. *)
let rec to_gui vars v pt expl_opt = match vars, expl_opt with
  | [], _ -> Pdt.Leaf pt
  | x :: vars, None ->
     Node (x, [(Setc.Finite (Set.of_list (module Dom) [(Map.find_exn v x)]), to_gui vars v pt None)])
  | x :: vars, Some(Pdt.Node (y, part)) when String.equal x y ->
     let d = Map.find_exn v x in
     let sub = Setc.Finite (Set.of_list (module Dom) [d]) in
     match Part.find_remove part d with
     | None -> Node (x, (sub, to_gui vars v pt None) :: part)
     | Some(part, e) -> Node (x, (sub, to_gui vars v pt (Some(e))) :: part)
  | _ -> raise (Invalid_argument (Printf.sprintf "could not construct GUI-explanation"))
