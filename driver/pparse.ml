(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Format

type error =
  | CannotRun of string
  | WrongMagic of string

exception Error of error

(* Optionally preprocess a source file *)

let preprocess sourcefile =
  match !Clflags.preprocessor with
    None -> sourcefile
  | Some pp ->
      let tmpfile = Filename.temp_file "ocamlpp" "" in
      let comm = Printf.sprintf "%s %s > %s"
                                pp (Filename.quote sourcefile) tmpfile
      in
      if Ccomp.command comm <> 0 then begin
        Misc.remove_file tmpfile;
        raise (Error (CannotRun comm));
      end;
      tmpfile

let remove_preprocessed inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> Misc.remove_file inputfile

let remove_preprocessed_if_ast inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ ->
      if inputfile <> !Location.input_name then Misc.remove_file inputfile

(* Parse a file or get a dumped syntax tree in it *)

exception Outdated_version

let write_ast magic ast =
  let fn = Filename.temp_file "camlppx" "" in
  let oc = open_out_bin fn in
  output_string oc magic;
  output_value oc !Location.input_name;
  output_value oc ast;
  close_out oc;
  fn

let apply_rewriter magic fn_in ppx =
  let fn_out = Filename.temp_file "camlppx" "" in
  let comm = Printf.sprintf "%s %s %s" ppx (Filename.quote fn_in) (Filename.quote fn_out) in
  let ok = Ccomp.command comm = 0 in
  Misc.remove_file fn_in;
  if not ok then begin
    Misc.remove_file fn_out;
    raise (Error (CannotRun comm));
  end;
  if not (Sys.file_exists fn_out) then raise (Error (WrongMagic comm));
  (* check magic before passing to the next ppx *)
  let ic = open_in_bin fn_out in
  let buffer = try Misc.input_bytes ic (String.length magic) with End_of_file -> "" in
  close_in ic;
  if buffer <> magic then begin
    Misc.remove_file fn_out;
    raise (Error (WrongMagic comm));
  end;
  fn_out

let read_ast magic fn =
  let ic = open_in_bin fn in
  try
    let buffer = Misc.input_bytes ic (String.length magic) in
    assert(buffer = magic); (* already checked by apply_rewriter *)
    Location.input_name := input_value ic;
    let ast = input_value ic in
    close_in ic;
    Misc.remove_file fn;
    ast
  with exn ->
    close_in ic;
    Misc.remove_file fn;
    raise exn

let apply_rewriters magic ast =
  match !Clflags.ppx with
  | [] -> ast
  | ppxs ->
      let fn = List.fold_left (apply_rewriter magic) (write_ast magic ast) ppxs in
      read_ast magic fn

let file ppf inputfile parse_fun ast_magic =
  let ic = open_in_bin inputfile in
  let is_ast_file =
    try
      let buffer = Misc.input_bytes ic (String.length ast_magic) in
      if buffer = ast_magic then true
      else if String.sub buffer 0 9 = String.sub ast_magic 0 9 then
        raise Outdated_version
      else false
    with
      Outdated_version ->
        Misc.fatal_error "OCaml and preprocessor have incompatible versions"
    | _ -> false
  in
  let ast =
    try
      if is_ast_file then begin
        if !Clflags.fast then
          fprintf ppf "@[Warning: %s@]@."
            "option -unsafe used with a preprocessor returning a syntax tree";
        Location.input_name := input_value ic;
        input_value ic
      end else begin
        seek_in ic 0;
        Location.input_name := inputfile;
        let lexbuf = Lexing.from_channel ic in
        Location.init lexbuf inputfile;
        parse_fun lexbuf
      end
    with x -> close_in ic; raise x
  in
  close_in ic;
  apply_rewriters ast_magic ast


let report_error ppf = function
  | CannotRun cmd ->
      fprintf ppf "Error while running external preprocessor@.Command line: %s@." cmd
  | WrongMagic cmd ->
      fprintf ppf "External preprocessor does not produce a valid file@.Command line: %s@." cmd
