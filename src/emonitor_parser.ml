(*******************************************************************)
(*    This is part of WhyMyMon, and it is distributed under the    *)
(*     terms of the GNU Lesser General Public License version 3    *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2024:                                                *)
(*  Leonardo Lima (UCPH)                                           *)
(*******************************************************************)

open Base
open Etc

module Parsebuf = struct

  type t = { lexbuf: Lexing.lexbuf
           ; mutable token: Emonitor_lexer.token
           ; mutable tp: timepoint
           ; mutable ts: timestamp option
           ; mutable sss: (string list) list }

  let init lexbuf = { lexbuf = lexbuf
                    ; token = Emonitor_lexer.token lexbuf
                    ; tp = -1
                    ; ts = None
                    ; sss = [] }

  let next pb = pb.token <- Emonitor_lexer.token pb.lexbuf

  let clean pb = { pb with tp = -1; ts = None; sss = [] }

  let update_sss pb ss = pb.sss <- List.cons ss pb.sss

end

module Monpoly = struct

  type cursor = Processed of Parsebuf.t
              | Error     of Parsebuf.t * string

  let string_of_token (t: Emonitor_lexer.token) =
    match t with
    | AT -> "'@'"
    | LPA -> "'('"
    | RPA -> "')'"
    | COM -> "','"
    | SEP -> "';'"
    | STR s -> "\"" ^ String.escaped s ^ "\""
    | EOF -> "<EOF>"

  let parse_aux (pb: Parsebuf.t) =
    let rec parse_init () =
      match pb.token with
      | AT -> Parsebuf.next pb; parse_ts ()
      | EOF -> Processed pb
      | t -> Error (pb, "expected '@' but found " ^ string_of_token t)
    and parse_ts () =
      match pb.token with
      | STR s -> let ts = try Some (Int.of_string s)
                          with _ -> None in
                 (match ts with
                  | Some ts -> pb.ts <- Some ts;
                               Parsebuf.next pb;
                               parse_tp ()
                  | None -> Error (pb, "expected a time-stamp but found " ^ s))
      | t -> Error (pb, "expected a time-stamp but found " ^ string_of_token t)
    and parse_tp () =
      match pb.token with
      | LPA | RPA -> Parsebuf.next pb; parse_tp ()
      | COL -> Parsebuf.next pb; parse_tuple ()
      | STR s -> if String.equal s "time" || String.equal s "point" then (Parsebuf.next pb; parse_tp ())
                 else (let tp = try Some (Int.of_string s)
                                with _ -> None in
                       (match tp with
                        | Some tp -> pb.tp <- tp;
                                     Parsebuf.next pb;
                                     parse_tp ()
                        | None -> Error (pb, "expected a time-point but found " ^ s)))
      | t -> Error (pb, "expected a time-point but found " ^ string_of_token t)
    and parse_tuple () =
      match pb.token with
      | LPA -> Parsebuf.next pb; parse_tuple ()
      | RPA -> parse_tuple_cont (Queue.create ())
      | TRUE -> Processed pb
      | STR s -> Parsebuf.next pb;
                 parse_tuple_cont (Queue.of_list [s])
      | t -> Error (pb, "expected a tuple or ')' but found " ^ string_of_token t)
    and parse_tuple_cont q =
      match pb.token with
      | RPA -> Parsebuf.next pb;
               Parsebuf.update_sss pb (Queue.to_list q);
               (match pb.token with
                | LPA -> Parsebuf.next pb; parse_tuple ()
                | _ -> Processed pb)
      | COM -> Parsebuf.next pb;
               (match pb.token with
                | STR s -> Parsebuf.next pb;
                           Queue.enqueue q s;
                           parse_tuple_cont q
                | t -> Error (pb, "expected a tuple but found " ^ string_of_token t))
      | t -> Error (pb, "expected ',' or ')' but found " ^ string_of_token t) in
    parse_init ()

  let parse line =
    let pb = Parsebuf.init (Lexing.from_string line) in
    match parse_aux pb with
    | Processed pb -> (pb.tp, pb.ts, pb.sss)
    | Error (_, s) -> raise (Invalid_argument s)

end



module Timelymon = struct 
    type cursor = Processed of Parsebuf.t
              | Error     of Parsebuf.t * string

  let string_of_token (t: Emonitor_lexer.token) =
    match t with
    | LPA -> "'('"
    | RPA -> "')'"
    | COM -> "','"
    | SEP -> "';'"
    | STR s -> "\"" ^ String.escaped s ^ "\""
    | EOF -> "<EOF>"


    let parse_aux (pb: Parsebuf.t) =
    let rec parse_init () =
      match pb.token with
      | LPA -> Parsebuf.next pb; parse_tuple ()
      | EOF -> Processed pb
      | t -> Error (pb, "expected '(' but found " ^ string_of_token t)
  
      and parse_tuple () =
      match pb.token with
      | LPA -> Parsebuf.next pb; parse_tuple ()
      | RPA -> parse_tuple_cont (Queue.create ())
      | TRUE -> Processed pb
      | STR s -> Parsebuf.next pb;
                 parse_tuple_cont (Queue.of_list [s])
      | t -> Error (pb, "expected a tuple or ')' but found " ^ string_of_token t)

    and parse_tuple_cont q =
      match pb.token with
      | RPA -> Parsebuf.next pb;
               Parsebuf.update_sss pb (Queue.to_list q);
               parse_tuple_cont q
      | COL -> Parsebuf.next pb ; parse_tp()
      | COM -> Parsebuf.next pb;
               (match pb.token with
                | STR s -> Parsebuf.next pb;
                           Queue.enqueue q s;
                           parse_tuple_cont q
                | t -> Error (pb, "expected a tuple but found " ^ string_of_token t))
      | t -> Error (pb, "expected ',' or ')' but found " ^ string_of_token t)

      and parse_tp () =
      match pb.token with
      | LPA | RPA -> Parsebuf.next pb; parse_tp ()
      | STR s -> if String.equal s "Time" || String.equal s "point" || String.equal s "s" then (Parsebuf.next pb; parse_tp ())
                 else if (String.contains s '-') then
                  (match String.lsplit2 s ~on:'-' with 
                      | Some (l,r) -> 
                        let range =
                        try Some (Int.of_string l, Int.of_string r)
                                with _ -> None in
                        match (range) with
                        | Some (tp, ts) ->
                           pb.tp <- tp;
                           pb.ts <- Some ts;
                           Parsebuf.next pb;
                           Processed pb
                        | None -> Error (pb, "expected a time-point range but found " ^ s))
                else
                 let tp = try Some (Int.of_string s)
                                with _ -> None in
                       (match tp with
                        | Some tp -> pb.tp <- tp;
                                     Parsebuf.next pb;
                                     parse_tp ()
                        | None -> Error (pb, "expected a time-point but found " ^ s))
      | EOF -> Processed pb
      | t -> Error (pb, "expected a time-point but found" ^ string_of_token t) in
      parse_init ()
      


    let parse line =
    let pb = Parsebuf.init (Lexing.from_string line) in
    match parse_aux pb with
    | Processed pb -> (pb.tp, pb.ts, pb.sss)
    | Error (_, s) -> raise (Invalid_argument s)

end

module DejaVu = struct
   type cursor = Processed of Parsebuf.t
              | Error     of Parsebuf.t * string

  let string_of_token (t: Emonitor_lexer.token) =
    match t with
    | AT -> "'@'"
    | LPA -> "'('"
    | RPA -> "')'"
    | COM -> "','"
    | SEP -> "';'"
    | STR s -> "\"" ^ String.escaped s ^ "\""
    | EOF -> "<EOF>"

  let parse_aux (pb: Parsebuf.t) =
    let rec parse_init () =
      match pb.token with
      | STR s when String.equal s "number" -> parse_tp ();
      | STR s -> Parsebuf.next pb; parse_init()
      | EOF -> Processed pb
      | t -> Error (pb, "expected '****' but found " ^ string_of_token t)
    and parse_tp () =
      match pb.token with
      | STR s -> if String.equal s "number" then (Parsebuf.next pb; parse_tp ())
                 else (let tp = try Some (Int.of_string s)
                                with _ -> None in
                       (match tp with
                        | Some tp -> pb.tp <- tp;
                                     Parsebuf.next pb;
                                     Processed pb
                        | None -> Error (pb, "expected a time-point but found " ^ s)))
      | t -> Error (pb, "expected a time-point but found " ^ string_of_token t) in
      parse_init()
    let parse line =
      let pb = Parsebuf.init (Lexing.from_string line) in
    match parse_aux pb with
    | Processed pb -> (pb.tp - 1, None, pb.sss)
    | Error (_, s) -> raise (Invalid_argument s)
end