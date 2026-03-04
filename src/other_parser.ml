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
open Stdio
open Etc

let string_of_token (t: Other_lexer.token) =
  match t with
  | AT -> "'@'"
  | LPA -> "'('"
  | RPA -> "')'"
  | COM -> "','"
  | SEP -> "';'"
  | STR s -> "\"" ^ String.escaped s ^ "\""
  | EOF -> "<EOF>"

module Parsebuf = struct

  type t = { lexbuf: Lexing.lexbuf
           ; mutable token: Other_lexer.token
           ; mutable pred_sig: Pred.Sig.t option
           ; mutable ts: timestamp
           ; mutable db: Db.t }

  let init lexbuf = { lexbuf = lexbuf
                    ; token = Other_lexer.token lexbuf
                    ; pred_sig = None
                    ; ts = -1
                    ; db = Db.create [] }

  let next pb = pb.token <- Other_lexer.token pb.lexbuf

  let arity pb = (snd (Option.value_exn pb.pred_sig)).arity

  let pred pb = fst (Option.value_exn pb.pred_sig)

  let clean pb = { pb with pred_sig = None
                         ; ts = -1
                         ; db = Db.create [] }

  let add_event evt pb = pb.db <- Db.add_event pb.db evt

end

module Sig = struct

  let token_equal (t1: Other_lexer.token) (t2: Other_lexer.token) =
    match t1, t2 with
    | AT, AT
      | LPA, LPA
      | RPA, RPA
      | COM, COM
      | SEP, SEP
      | EOF, EOF -> true
    | STR s1, STR s2 -> String.equal s1 s2
    | _ -> false

  let parse_string (pb: Parsebuf.t) =
    match pb.token with
    | STR s -> Parsebuf.next pb; s
    | t -> raise (Failure ("expected a string but found " ^ string_of_token t))

  let parse_int pb =
    let s = parse_string pb in
    try Int.of_string s
    with Failure _ -> raise (Failure ("expected an integer but found \"" ^ String.escaped s ^ "\""))

  let parse_ntconst (pb: Parsebuf.t) =
    let expect (pb: Parsebuf.t) t =
      if token_equal pb.token t then Parsebuf.next pb
      else raise (Failure ("expected " ^ string_of_token t ^ " but found " ^ string_of_token pb.token)) in
    let rec parse_ntconst_rec l =
      match pb.token with
      | COM -> Parsebuf.next pb;
               let s = parse_string pb in
               parse_ntconst_rec (s::l)
      | RPA -> Parsebuf.next pb; List.rev l
      | t -> raise (Failure ("expected ',' or ')' but found " ^ string_of_token t)) in
    expect pb LPA;
    match pb.token with
    | RPA -> Parsebuf.next pb; []
    | STR s -> Parsebuf.next pb; parse_ntconst_rec [s]
    | t -> raise (Failure ("expected a string or ')' but found " ^ string_of_token t))

  let convert_types sl =
    List.map sl ~f:(fun s -> match String.split s ~on:':' with
                             | [] -> raise (Failure ("unable to parse the variable signature string " ^ s))
                             | name :: ttype :: [] -> (name, Dom.tt_of_string ttype)
                             | _ -> raise (Failure ("unable to parse the variable signature string " ^ s)))

  let rec parse_pred_sigs (pb: Parsebuf.t) =
    match pb.token with
    | EOF -> ()
    | STR s -> Parsebuf.next pb;
               Pred.Sig.add s (convert_types (parse_ntconst pb));
               parse_pred_sigs pb
    | t -> raise (Failure ("unexpected character: " ^ string_of_token t))

  let parse_from_channel fn =
    let inc = In_channel.create fn in
    let lexbuf = Lexing.from_channel inc in
    let pb = Parsebuf.init lexbuf in
    let () = Lexing.set_filename lexbuf fn in
    try parse_pred_sigs pb
    with Failure s -> failwith ("error while parsing signature\n " ^ s)

  let parse_from_string ssig =
    let lexbuf = Lexing.from_string ssig in
    let pb = Parsebuf.init lexbuf in
    try parse_pred_sigs pb
    with Failure s -> failwith ("error while parsing signature\n " ^ s)

end

module Trace = struct

  type cursor = Processed of Parsebuf.t
              | Watermark of int
              | Skipped   of Parsebuf.t * string
              | Finished

  module Csv = struct
    let strip = String.strip

    let normalize_pred tok =
      let tok = strip tok in
      match String.lsplit2 tok ~on:'\'' with
      | Some (_, p) -> p
      | None -> tok

    let unquote_any s =
      let s = strip s in
      let len = String.length s in
      if len >= 2 then
        let first = s.[0] in
        let last = s.[len - 1] in
        if (Char.equal first '\'' && Char.equal last '\'')
           || (Char.equal first '\"' && Char.equal last '\"')
        then String.sub s ~pos:1 ~len:(len - 2)
        else s
      else s

    let parse_kv s =
      match String.lsplit2 (strip s) ~on:'=' with
      | Some (k, v) -> Some (strip k, strip v)
      | None -> None

    let parse_x_idx k =
      if String.is_prefix k ~prefix:"x"
      then Int.of_string_opt (String.drop_prefix k 1)
      else None

    let parse_event line =
      let line = strip line in
      if String.is_empty line then None
      else if String.is_substring line ~substring:"WATERMARK" then None
      else
        let parts = String.split line ~on:',' |> List.map ~f:strip in
        match parts with
        | pred_tok :: rest ->
            let pred = normalize_pred pred_tok in
            let kvs = List.filter_map rest ~f:parse_kv in
            let tp_opt =
              List.Assoc.find kvs ~equal:String.equal "tp"
              |> Option.bind ~f:Int.of_string_opt
            in
            let ts_opt =
              List.Assoc.find kvs ~equal:String.equal "ts"
              |> Option.bind ~f:Int.of_string_opt
            in
            let args =
              kvs
              |> List.filter_map ~f:(fun (k, v) ->
                     Option.map (parse_x_idx k) ~f:(fun i -> (i, unquote_any v)))
              |> List.sort ~compare:(fun (i, _) (j, _) -> Int.compare i j)
              |> List.map ~f:snd
            in
            (match tp_opt, ts_opt with
             | None, _
             | _, None -> None
             | Some tp, Some ts ->
                 (try
                    let evt = Db.Event.create pred args in
                    let db = Db.add_event (Db.create []) evt in
                    Some (tp, ts, db)
                  with _ -> None))
        | [] -> None

    let parse_line line =
      match parse_event line with
             | None -> None
             | Some (_, ts, db) -> Some (ts, db)

    let parse_watermark line =
      let line = strip line in
      if String.is_substring line ~substring:"WATERMARK" then
        match String.lsplit2 line ~on:' ' with
        | Some (_, rest) ->
            let rest = String.filter rest ~f:(fun c -> Char.is_digit c || Char.equal c '-') in
            Int.of_string_opt rest
        | None -> None
      else None
  end

  let parse_aux (pb: Parsebuf.t) =
    let rec parse_init () =
      match pb.token with
      | AT -> Parsebuf.next pb; parse_ts ()
      | EOF -> Finished
      | t -> Skipped (pb, "expected '@' but found " ^ string_of_token t)
    and parse_ts () =
      match pb.token with
      | STR s -> let ts = try Some (Int.of_string s)
                          with _ -> None in
                 (match ts with
                  | Some ts -> Parsebuf.next pb;
                               pb.ts <- ts;
                               parse_db ()
                  | None -> Skipped (pb, "expected a time-stamp but found " ^ s))
      | t -> Skipped (pb, "expected a time-stamp but found " ^ string_of_token t)
    and parse_db () =
      match pb.token with
      | STR s -> (match Hashtbl.find Pred.Sig.table s with
                  | Some props -> (pb.pred_sig <- Some(s, props);
                                   Parsebuf.next pb;
                                   (match pb.token with
                                    | LPA -> Parsebuf.next pb;
                                             parse_tuple ()
                                    | t -> Skipped (pb, "expected '(' but found " ^ string_of_token t)))
                  | None -> Skipped (pb, "predicate " ^ s ^ " was not specified"))
      | AT -> Processed pb
      | EOF -> Processed pb
      | SEP -> Parsebuf.next pb; Processed pb
      | t -> Skipped (pb, "expected a predicate or '@' but found " ^ string_of_token t)
    and parse_tuple () =
      match pb.token with
      | RPA -> parse_tuple_cont (Queue.create ())
      | STR s -> Parsebuf.next pb;
                 parse_tuple_cont (Queue.of_list [s])
      | t -> Skipped (pb, "expected a tuple or ')' but found " ^ string_of_token t)
    and parse_tuple_cont q =
      match pb.token with
      | RPA -> Parsebuf.next pb;
               (if Int.equal (Queue.length q) (Parsebuf.arity pb) then
                  let evt = Db.Event.create (Parsebuf.pred pb) (Queue.to_list q) in
                  Parsebuf.add_event evt pb;
                  (match pb.token with
                   | LPA -> Parsebuf.next pb; parse_tuple ()
                   | _ -> parse_db ())
                else Skipped (pb, Printf.sprintf "expected a tuple of arity %d but found %d arguments"
                                    (Parsebuf.arity pb) (Queue.length q)))
      | COM -> Parsebuf.next pb;
               (match pb.token with
                | STR s -> Parsebuf.next pb;
                           Queue.enqueue q s;
                           parse_tuple_cont q
                | t -> Skipped (pb, "expected a tuple but found " ^ string_of_token t))
      | t -> Skipped (pb, "expected ',' or ')' but found " ^ string_of_token t) in
    parse_init ()

  let parse_from_channel inc pb_opt =
    if !Etc.log_is_csv then
      let pb =
        match pb_opt with
        | None -> Parsebuf.init (Lexing.from_string "")
        | Some pb -> Parsebuf.clean pb
      in
      match In_channel.input_line inc with
      | None -> Finished
      | Some line ->
          (match Csv.parse_watermark line with
           | Some w -> Watermark w
           | None ->
               (match Csv.parse_line line with
                | None -> Skipped (pb, "csv line skipped")
                | Some (ts, db) ->
                    pb.ts <- ts;
                    pb.db <- db;
                    Processed pb))
    else if Option.is_none pb_opt then
      let lexbuf = Lexing.from_channel inc in
      parse_aux (Parsebuf.init lexbuf)
    else parse_aux (Parsebuf.clean (Option.value_exn pb_opt))

  let parse_from_string log =
    let lexbuf = Lexing.from_string log in
    parse_aux (Parsebuf.init lexbuf)

  let parse_line line =
    let lexbuf = Lexing.from_string line in
    match parse_aux (Parsebuf.init lexbuf) with
    | Processed pb -> Some (pb.ts, pb.db)
    | Watermark _ -> None
    | Skipped (_, s) -> None
    | Finished -> None

end

module CSV = struct
  type cursor = Processed of Parsebuf.t
              | Skipped   of Parsebuf.t * string
              | Watermark of int
              | Finished
end

