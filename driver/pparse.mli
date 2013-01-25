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

val preprocess : string -> string
val remove_preprocessed : string -> unit
val remove_preprocessed_if_ast : string -> unit
val file : formatter -> string -> (Lexing.lexbuf -> 'a) -> string -> 'a
val apply_rewriters : string -> 'a -> 'a
val report_error : formatter -> error -> unit
