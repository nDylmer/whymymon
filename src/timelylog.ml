open Base

(* TimelyMon expects explicit tp and ts per emitted fact line. *)
let tp_ref = ref (-1)

let reset () =
  tp_ref := -1

let next_tp () =
  tp_ref := !tp_ref + 1;
  !tp_ref

let atom_of_dom = function
  | Dom.Int i -> Int.to_string i
  | Dom.Str s -> Printf.sprintf "'%s'" s
  | Dom.Float f -> Printf.sprintf "'%s'" (Float.to_string f)

let event_line ~tp ~ts ((pred, args) : Db.Event.t) =
  let csv_args =
    args
    |> List.mapi ~f:(fun i d -> Printf.sprintf "x%d=%s" i (atom_of_dom d))
    |> String.concat ~sep:", "
  in
  if String.is_empty csv_args then
    Printf.sprintf "%s, tp=%d, ts=%d\n" pred tp ts
  else
    Printf.sprintf "%s, tp=%d, ts=%d, %s\n" pred tp ts csv_args

let encode_db ~tp ~ts (db : Db.t) =
  Set.to_list db
  |> List.map ~f:(event_line ~tp ~ts)
  |> String.concat ~sep:""

let encode_next_tp ~ts (db : Db.t) =
  let tp = next_tp () in
  encode_db ~tp ~ts db
