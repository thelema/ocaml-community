open Dim

type 'd bstring = string

external length : 'd bstring -> int = "%string_length"
external get : 'd bstring -> int -> char = "%string_safe_get"
external set : 'd bstring -> int -> char -> unit = "%string_safe_set"
external create : 'd dim -> 'd bstring = "caml_create_string"
external unsafe_get : 'd bstring -> int -> char = "%string_unsafe_get"
external unsafe_set : 'd bstring -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit : 'd bstring -> int -> 'd bstring -> int -> int -> unit
                     = "caml_blit_string" "noalloc"
external unsafe_fill : 'd bstring -> int -> int -> char -> unit
                     = "caml_fill_string" "noalloc"

let make d c = String.make (Dim.to_int d) c
let copy = String.copy
let sub = String.sub
let fill = String.fill
let blit = String.blit
let iter = String.iter
(* let concat =  *)
let of_list = String.of_list

(* let escaped = *)
let map = String.map
let uppercase = String.uppercase
let lowercase = String.lowercase

let capitalize = String.capitalize
let uncapitalize = String.uncapitalize

let index = String.index
let index_from = String.index_from
let rindex = String.rindex
let rindex_from = String.rindex_from

let contains_from = String.contains_from
let rcontains_from = String.rcontains_from
let contains = String.contains

let init d f = String.init (to_int d) f
let rev_map = String.rev_map
let rev_iter = String.rev_iter
let fold_left = String.fold_left
let fold_right = String.fold_right
let explode = String.explode
(* let implode = *)
(* let splice = String.splice *)

let starts_with = String.starts_with
let ends_with = String.ends_with
let find = String.find
let exists = String.exists

(* let strip = *)
(* let split = *)
(* let nsplit = *)



(* TODO
let to_enum = 

let of_enum = 
*)
