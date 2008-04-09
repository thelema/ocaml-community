open Dim

type 'd bstring

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

val make : 'd dim -> char -> 'd bstring
val copy : 'd bstring -> 'd bstring
val sub : 'd bstring -> int -> int -> string
val fill : 'd bstring -> int -> int -> char -> unit
val blit : 'd bstring -> int -> 'd bstring -> int -> int -> unit
val iter : (char -> unit) -> 'd bstring -> unit
(*val of_list : ( *)

val index : 'd bstring -> char -> int
val index_from : 'd bstring -> int -> char -> int
val rindex : 'd bstring -> char -> int
val rindex_from : 'd bstring -> int -> char -> int

val uppercase : 'd bstring -> 'd bstring
val lowercase : 'd bstring -> 'd bstring
val capitalize : 'd bstring -> 'd bstring
val uncapitalize: 'd bstring -> 'd bstring

val contains : 'd bstring -> char -> bool
val contains_from : 'd bstring -> int -> char -> bool
val rcontains_from : 'd bstring -> int -> char -> bool

val init : 'd dim -> (int -> char) -> 'd bstring
val map : (char -> char) -> 'd bstring -> 'd bstring
val rev_map : (char -> char) -> 'd bstring -> 'd bstring
val rev_iter : (char -> unit) -> 'd bstring -> unit
val fold_left : ('a -> char -> 'a) -> 'a -> 'd bstring -> 'a
val fold_right : (char -> 'a -> 'a) -> 'd bstring -> 'a -> 'a
val explode : 'd bstring -> char list

val starts_with : 'd bstring -> string -> bool
val ends_with : 'd bstring -> string -> bool
val find : 'd bstring -> string -> int
val exists : 'd bstring -> string -> bool

