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
  let get_tp pb = pb.tp

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


  let parse_aux (pb: Parsebuf.t) =
  let rec parse_init () =
    match pb.token with
    | STR raw ->
        let pred_name = predicate raw in
        (match Hashtbl.find Pred.Sig.table pred_name with
         | Some props ->
             pb.pred_sig <- Some (pred_name, props);
             Parsebuf.next pb;
             parse_tp();
         | None ->
             Skipped (pb, "predicate " ^ pred_name ^ " was not specified"))
    | EOF -> Finished
    | t -> Skipped (pb, "expected a predicate but found " ^ string_of_token t)
  and parse_tp () =
  match pb.token with
  | STR s when String.equal s "tp" ->
      Parsebuf.next pb;                                
      (match pb.token with
       | EQ ->
           Parsebuf.next pb;                           
           (match pb.token with
            | STR num_s ->
                (match Int.of_string_opt num_s with
                 | Some tp ->
                     pb.tp <- tp;
                     Parsebuf.next pb;                
                     parse_ts()              
                 | None -> Skipped (pb, "expected an integer but found " ^ num_s))
            | t -> Skipped (pb, "expected an integer but found " ^ string_of_token t))
       | t -> Skipped (pb, "expected '=' but found " ^ string_of_token t))
  | COM -> Parsebuf.next pb; parse_tp();
  | t -> Skipped (pb, "expected 'tp' but found " ^ string_of_token t)

  and parse_ts () =
  match pb.token with
  | STR s when String.equal s "ts" ->
      Parsebuf.next pb;
      (match pb.token with
       | EQ ->
           Parsebuf.next pb;
           (match pb.token with
            | STR num_s ->
                (match Int.of_string_opt num_s with
                 | Some ts ->
                     pb.ts <- Some ts;
                     Parsebuf.next pb;
                     parse_args (Queue.create ())
                 | None -> Skipped (pb, "expected an integer but found " ^ num_s))
            | t -> Skipped (pb, "expected an integer but found " ^ string_of_token t))
       | t -> Skipped (pb, "expected '=' but found " ^ string_of_token t))
  | COM -> Parsebuf.next pb; parse_ts();
  | t -> Skipped (pb, "expected 'ts' but found " ^ string_of_token t)

  and parse_args q =
  match pb.token with
  | EOF  ->
      let evt = Db.Event.create (Parsebuf.pred pb) (Queue.to_list q) in
      Parsebuf.add_event evt pb;
      Processed pb
  | COM ->
      Parsebuf.next pb;
      (match pb.token with
       | STR key ->
           Parsebuf.next pb;
           (match pb.token with
            | EQ ->
                Parsebuf.next pb;
                (match pb.token with
                 | STR v ->
                     Parsebuf.next pb;
                     Queue.enqueue q v;
                     parse_args q                      
                 | t -> Skipped (pb, "expected a value but found " ^ string_of_token t))
            | t -> Skipped (pb, "expected '=' but found " ^ string_of_token t))
       | t -> Skipped (pb, "expected a key but found " ^ string_of_token t))
  | t -> Skipped (pb, "expected ',' or end of event but found " ^ string_of_token t)
                 in 
  parse_init()

  let parse_from_channel inc _pb_opt =
  match In_channel.input_line inc with
  | None -> Finished
  | Some line ->
      let line = String.strip line in
      if String.is_empty line then
        Skipped (Parsebuf.init (Lexing.from_string ""), "empty line")
      else if String.is_substring line ~substring:"WATERMARK" then
        (let digits = String.filter line ~f:Char.is_digit in
         match Int.of_string_opt digits with
         | Some w -> Watermark w
         | None   -> Skipped (Parsebuf.init (Lexing.from_string ""),
                              "bad watermark line"))
      else
        parse_aux (Parsebuf.init (Lexing.from_string line))

end


module CSV_dejavu = struct

  type cursor = Processed of Parsebuf.t
              | Skipped   of Parsebuf.t * string
              | Watermark of int
              | Finished

let parse_aux (pb: Parsebuf.t) =
  let rec parse_init () =
    match pb.token with
    | STR s ->
             Parsebuf.next pb;
             parse_args s (Queue.create());
    | EOF -> Finished
    | t -> Skipped (pb, "expected a predicate but found " ^ string_of_token t)
  and parse_args pred q =
    match pb.token with
    | EOF ->
        let all = Queue.to_list q in
        let (args, ts_opt) =
          if !Etc.is_dejavu_timed then
            (match List.rev all with
             | last :: rest_rev -> (List.rev rest_rev, Int.of_string_opt last)
             | [] -> ([], None))
          else
            (all, None)
        in
        let evt = (pred, List.map args ~f:(fun s -> Dom.Str s)) in
        pb.tp <- pb.tp + 1;
        pb.ts <- ts_opt;
        Parsebuf.add_event evt pb;
        Processed pb
    | COM ->
        Parsebuf.next pb;
        (match pb.token with
         | STR v ->
             Parsebuf.next pb;
             Queue.enqueue q v;
             parse_args pred q
         | t -> Skipped (pb, "expected an argument but found " ^ string_of_token t))
    | t -> Skipped (pb, "expected ',' or end of event but found " ^ string_of_token t)
  in
  parse_init ()



  let parse_from_channel inc pb_opt =
  let prev_tp = match pb_opt with
    | None -> -1                      
    | Some pb -> Parsebuf.get_tp pb    
  in
  match In_channel.input_line inc with
  | None -> Finished
  | Some line ->
      let line = String.strip line in
      let pb = Parsebuf.init (Lexing.from_string line) in
      pb.tp <- prev_tp;               
      if String.is_empty line then
        Skipped (pb, "empty line")
      else
        parse_aux pb 
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
  match pb_opt with
  | None ->
      let lexbuf = Lexing.from_channel inc in
      parse_aux (Parsebuf.init lexbuf)
  | Some prev ->
      parse_aux (Parsebuf.clean prev)
end



module Event_stream = struct

  type cursor = Processed of Parsebuf.t
              | Skipped   of Parsebuf.t * string
              | Watermark of int
              | Finished

  let parse inc pb_opt mon =
    match mon with
    | Argument.Monitor.DejaVu ->
        (match CSV_dejavu.parse_from_channel inc pb_opt with
         | CSV_dejavu.Processed pb     -> Processed pb
         | CSV_dejavu.Skipped (pb, m)  -> Skipped (pb, m)
         | CSV_dejavu.Watermark w      -> Watermark w
         | CSV_dejavu.Finished         -> Finished)
    | TimelyMon when !Etc.log_is_csv ->
        (match CSV.parse_from_channel inc pb_opt with
         | CSV.Processed pb    -> Processed pb
         | CSV.Skipped (pb, m) -> Skipped (pb, m)
         | CSV.Watermark w     -> Watermark w
         | CSV.Finished        -> Finished)
    | MonPoly | TimelyMon | VeriMon ->
        (match Trace.parse_from_channel inc pb_opt with
         | Trace.Processed pb    -> Processed pb
         | Trace.Skipped (pb, m) -> Skipped (pb, m)
         | Trace.Watermark w     -> Watermark w
         | Trace.Finished        -> Finished)

end
