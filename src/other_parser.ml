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
           ; mutable tp : timepoint
           ; mutable ts: timestamp option
           ; mutable db: Db.t }

  let init lexbuf = { lexbuf = lexbuf
                    ; token = Other_lexer.token lexbuf
                    ; pred_sig = None
                    ; tp = -1
                    ; ts = None
                    ; db = Db.create [] }

  let next pb = pb.token <- Other_lexer.token pb.lexbuf

  let arity pb = (snd (Option.value_exn pb.pred_sig)).arity

  let pred pb = fst (Option.value_exn pb.pred_sig)

  let clean pb = { pb with pred_sig = None
                         ; ts = None
                         ; tp = -1
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

module CSV = struct
  type cursor = Processed of Parsebuf.t
              | Skipped   of Parsebuf.t * string
              | Watermark of int
              | Finished

  let predicate (s : string) : string =
    let s = String.strip s in
    match String.lsplit2 s ~on:'\'' with
    | Some (_,p) -> p
    | _ -> s

  let parse_key_value s =
  match String.lsplit2 (String.strip s) ~on:'=' with
  | Some (k, v) -> Some (String.strip k, String.strip v)
  | None -> None


  let parse_event line =
    let line = String.strip line in
    if String.is_empty line || String.is_substring line ~substring:"WATERMARK" then None
    else
        let parts = List.map (String.split line ~on:',') ~f:String.strip in
        match parts with
        | [] -> None
        | pred :: rest -> 
          let key_values = List.filter_map rest ~f:parse_key_value in
          let tp_optional = 
              match List.Assoc.find key_values ~equal:String.equal "tp" with
            | None -> None
            | Some s -> Int.of_string_opt s
          in
          let ts_optional = 
              match List.Assoc.find key_values ~equal:String.equal "ts" with
            | None -> None
            | Some s -> Int.of_string_opt s
          in
          let pred = predicate pred in
          let args =
            let values =
              List.filter key_values ~f:(fun (k, _) ->
              not (String.equal k "tp" || String.equal k "ts"))
            in
            List.map values ~f:snd
          in 
           (match tp_optional with
            | Some tp ->
              (try
                let event = Db.Event.create pred args in
                let db = Db.add_event (Db.create []) event in
                Some (tp, ts_optional, db)
              with _ -> None)
            | _ -> None)
        
  
  let parse_from_channel inc pb_opt =
    let pb =
      if Option.is_none pb_opt then 
        Parsebuf.init (Lexing.from_string "")
      else Parsebuf.clean  (Option.value_exn pb_opt)
    in
    match In_channel.input_line inc with 
    | None -> Finished
    | Some line ->
      let line = String.strip line in
      if String.is_empty line then
        Skipped (pb, "empty line")
      else if String.is_substring line ~substring:"WATERMARK" then
        let wm_opt =
          let digits =
            String.filter line ~f:(fun x -> Char.is_digit x)
          in
          Int.of_string_opt digits
        in
        (match wm_opt with
        | Some w -> Watermark w
        | None -> Skipped (pb, "bad watermark line"))
      else 
        match parse_event line with
        | Some (tp,ts_opt,db) ->
            pb.tp <- tp;    
            pb.ts <- ts_opt;
            pb.db <- db;
            Processed pb
        | None -> 
          Skipped (pb, "bad csv event")
      
end

module Trace = struct

  type cursor = Processed of Parsebuf.t
              | Skipped   of Parsebuf.t * string
              | Watermark of int
              | Finished

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
                               pb.ts <- Some ts;
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
      match CSV.parse_from_channel inc pb_opt with
      | CSV.Processed pb -> Processed pb
      | CSV.Skipped (pb, msg) -> Skipped (pb, msg)
      | CSV.Watermark w -> Watermark w
      | CSV.Finished -> Finished
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
    | Skipped (_, s) -> None
    | Finished -> None
    | Watermark w -> None

end




(*module CSV_dejavu = struct

  type cursor = Processed of Parsebuf.t
              | Skipped   of Parsebuf.t * string
              | Watermark of int
              | Finished

  let parse_aux (pb: Parsebuf.t) =
    let rec parse_init () =
      match pb.token with
      | STR s -> Parsebuf.next pb; parse_db ()
      | EOF -> Finished
      | t -> Skipped (pb, "expected a string" ^ string_of_token t)
    and parse_ts () = *)