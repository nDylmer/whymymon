(*******************************************************************)
(*    This is part of WhyMyMon, and it is distributed under the    *)
(*     terms of the GNU Lesser General Public License version 3    *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2024:                                                *)
(*  Dmitriy Traytel (UCPH)                                         *)
(*  Leonardo Lima (UCPH)                                           *)
(*******************************************************************)

open Base
open Stdio
open Monitor_lib
open Monitor_lib.Etc

(* TODO: This module must be rewritten using the Command module from Core *)
module WhyMyMon = struct

  let mon_path_ref = ref ""
  let sig_path_ref = ref ""
  let formula_file_ref = ref ""

  let interf_ref = ref Argument.Interface.GUI
  let mon_ref = ref Argument.Monitor.MonPoly
  let pref_ref = ref Argument.Preference.Violation
  let mode_ref = ref Argument.Mode.Unverified
  let formula_ref = ref None
  let stream_ref = ref Stdio.In_channel.stdin
  let logstr_ref = ref ""

  let nec_arg_count = ref 0

  let usage () =
    Format.eprintf
      "usage: whymymon.exe -path -sig -formula [-monitor] [-path] [-pref] [-log]
       arguments:
       \t -gui                 - explanations are sent to the GUI (default)
       \t -cli                 - explanations are printed on the CLI
       \t -monitor
       \t\t monpoly            - (default)
       \t\t dejavu
       \t\t timelymon
       \t\t verimon
       \t -path
       \t\t default            - (default) path (folder third-party) for external monitors
       \t\t <file>             - chosen monitor's executable full path
       \t -pref
       \t\t vio                - explain violations (default)
       \t\t sat                - explain satisfactions
       \t -mode
       \t\t unverified         - (default)
       \t\t verified           - check output with formally verified checker
       \t -sig
       \t\t <file>             - signature
       \t -formula
       \t\t <file>             - MFOTL formula
       \t -log
       \t\t <file> or stdin    - specify a log file as a stream (default: stdin)\n%!";
    exit 0

  let process_args =
    let rec process_args_rec = function
      | ("-gui" :: args) ->
         interf_ref := Argument.Interface.GUI;
         process_args_rec args
      | ("-cli" :: args) ->
         interf_ref := Argument.Interface.CLI;
         process_args_rec args
      | ("-monitor" :: m :: args) ->
         mon_ref := Argument.Monitor.of_string m;
         process_args_rec args
      | ("-path" :: p :: args) ->
         mon_path_ref := (if String.equal p "default" then
                            (Filename_unix.realpath (Argument.Monitor.exec_path !mon_ref))
                          else Filename_unix.realpath p);
         process_args_rec args
      | ("-pref" :: p :: args) ->
         pref_ref := Argument.Preference.of_string p;
         process_args_rec args
      | ("-mode" :: m :: args) ->
         mode_ref := Argument.Mode.of_string m;
         if String.equal m "debug" then Etc.debug := true;
         process_args_rec args
      | ("-sig" :: sf :: args) ->
         nec_arg_count := !nec_arg_count + 1;
         sig_path_ref := Filename_unix.realpath sf;
         Other_parser.Sig.parse_from_channel sf;
         process_args_rec args
      | ("-formula" :: f :: args) ->
         nec_arg_count := !nec_arg_count + 1;
         formula_file_ref := (match (String.rindex f '/') with
                              | None -> f
                              | Some ri -> String.suffix f ((String.length f) - ri - 1));
         In_channel.with_file f ~f:(fun inc ->
             let lexbuf = Lexing.from_channel inc in
             formula_ref := try Some(Formula_parser.formula Formula_lexer.token lexbuf)
                            with Formula_parser.Error ->
                              Stdio.printf "%s\n" (Etc.lexbuf_error_msg lexbuf);
                              Stdlib.flush_all (); None);
         process_args_rec args
      | ("-log" :: f :: args) ->
         stream_ref := In_channel.create f;
         process_args_rec args
      | ("-logstr" :: logs :: args) ->
         logstr_ref := logs;
         process_args_rec args
      | [] -> if !nec_arg_count >= 2 then () else usage ()
      | _ -> usage () in
    process_args_rec

  let _ =
    try
      process_args (List.tl_exn (Array.to_list Sys.argv));
      let extra_args = Argument.Monitor.extra_args !pref_ref !mon_ref in
      if String.equal !mon_path_ref "" then
        mon_path_ref := Filename_unix.realpath (Argument.Monitor.exec_path !mon_ref);
      match !mon_ref with
      | MonPoly
        | VeriMon -> let _ = Monitor.exec !interf_ref !mon_ref ~mon_path:!mon_path_ref ~sig_path:!sig_path_ref
                               ~formula_file:!formula_file_ref !stream_ref (Option.value_exn !formula_ref)
                               !pref_ref !mode_ref extra_args in ()

      | DejaVu -> failwith "not yet"
      | TimelyMon -> failwith "not yet"
      | _ -> failwith "not yet"
    with End_of_file -> exit 0

end
