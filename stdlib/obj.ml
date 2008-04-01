(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(* portions lifted from Extlib                                         *)
(* Copyright (C) 2003 Brian Hurt                                       *)
(* Copyright (C) 2003 Nicolas Cannasse                                 *)
(***********************************************************************)

(* $Id$ *)

(* Operations on internal representations of values *)

type t

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
external is_block : t -> bool = "caml_obj_is_block"
external is_int : t -> bool = "%obj_is_int"
external tag : t -> int = "caml_obj_tag"
external set_tag : t -> int -> unit = "caml_obj_set_tag"
external size : t -> int = "%obj_size"
external field : t -> int -> t = "%obj_field"
external set_field : t -> int -> t -> unit = "%obj_set_field"
external new_block : int -> int -> t = "caml_obj_block"
external dup : t -> t = "caml_obj_dup"
external truncate : t -> int -> unit = "caml_obj_truncate"

let marshal (obj : t) =
  Marshal.to_string obj []
let unmarshal str pos =
  (Marshal.from_string str pos, pos + Marshal.total_size str pos)

let lazy_tag = 246
let closure_tag = 247
let object_tag = 248
let infix_tag = 249
let forward_tag = 250

let no_scan_tag = 251

let abstract_tag = 251
let string_tag = 252
let double_tag = 253
let double_array_tag = 254
let custom_tag = 255
let final_tag = custom_tag


let int_tag = 1000
let out_of_heap_tag = 1001

let rec dump r =
  if is_int r then
    string_of_int (magic r : int)
  else (* Block. *)
    let rec get_fields acc = function
      | 0 -> acc
      | n -> let n = n-1 in get_fields (field r n :: acc) n
    in
    let rec is_list r =
      if is_int r then
	r = repr 0 (* [] *)
      else
	let s = size r and t = tag r in
	t = 0 && s = 2 && is_list (field r 1) (* h :: t *)
	in
    let rec get_list r =
      if is_int r then
	[]
      else 
	let h = field r 0 and t = get_list (field r 1) in
	h :: t
    in
    let opaque name =
      (* XXX In future, print the address of value 'r'.  Not possible in
       * pure OCaml at the moment.
       *)
      "<" ^ name ^ ">"
    in
    let s = size r and t = tag r in
    (* From the tag, determine the type of block. *)
    match t with 
      | _ when is_list r ->
	  let fields = get_list r in
	  "[" ^ String.concat "; " (List.map dump fields) ^ "]"
      | 0 ->
	  let fields = get_fields [] s in
	  "(" ^ String.concat ", " (List.map dump fields) ^ ")"
      | x when x = lazy_tag ->
	  (* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
	   * clear if very large constructed values could have the same
	   * tag. XXX *)
	  opaque "lazy"
      | x when x = closure_tag ->
	  opaque "closure"
      | x when x = object_tag ->
	  let fields = get_fields [] s in
	  let clasz, id, slots =
	    match fields with
	      | h::h'::t -> h, h', t 
	      | _ -> assert false
	  in
	  (* No information on decoding the class (first field).  So just print
	   * out the ID and the slots. *)
	  "Object #" ^ dump id ^ " (" ^ String.concat ", " (List.map dump slots) ^ ")"
      | x when x = infix_tag ->
	  opaque "infix"
      | x when x = forward_tag ->
	  opaque "forward"
      | x when x < no_scan_tag ->
	  let fields = get_fields [] s in
	  "Tag" ^ string_of_int t ^
	    " (" ^ String.concat ", " (List.map dump fields) ^ ")"
      | x when x = string_tag ->
	  "\"" ^ String.escaped (magic r : string) ^ "\""
      | x when x = double_tag ->
	  string_of_float (magic r : float)
      | x when x = abstract_tag ->
	  opaque "abstract"
      | x when x = custom_tag ->
	  opaque "custom"
      | x when x = final_tag ->
	  opaque "final"
      | _ ->
	  failwith ("Std.dump: impossible tag (" ^ string_of_int t ^ ")")

let dump v = dump (repr v)

let print v = print_endline (dump v)

let __unique_counter = ref 0
  
let unique_int () =
  incr __unique_counter;
  !__unique_counter
