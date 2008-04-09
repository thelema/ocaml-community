open Dim

type ('t, 'd) barray

val make : 'd dim -> 't -> ('t, 'd) barray
val init : 'd dim -> (int -> 'a) -> ('a, 'd) barray
val copy : ('a, 'd) barray -> ('a, 'd) barray
  (* other array operations go here ... *)
val get : ('a, 'd) barray -> int -> 'a
val set : ('a, 'd) barray -> int -> 'a -> unit
val combine :
  ('a, 'd) barray -> ('b, 'd) barray -> ('a -> 'b -> 'c) ->
  ('c, 'd) barray
val length : ('a, 'd) barray -> int
val update : ('a, 'd) barray -> int -> 'a -> ('a, 'd) barray
val iter : ('a -> unit) -> ('a, 'd) barray -> unit
val map : ('a -> 'b) -> ('a, 'd) barray -> ('b, 'd) barray
val iteri : (int -> 'a -> unit) -> ('a, 'd) barray -> unit
val mapi : (int -> 'a -> 'b) -> ('a, 'd) barray -> ('b, 'd) barray
val fold_left : ('a -> 'b -> 'a) -> 'a -> ('b,'d) barray -> 'a
val fold_right : ('b -> 'a -> 'a) -> ('b, 'd) barray -> 'a -> 'a
    
val iter2 :
  ('a -> 'b -> unit) -> ('a,'d) barray -> ('b, 'd) barray -> unit
val map2 :
  ('a -> 'b -> 'c) -> ('a, 'd) barray -> ('b, 'd) barray -> ('c, 'd) barray
val iteri2 :
  (int -> 'a -> 'b -> unit) -> ('a,'d) barray -> ('b, 'd) barray ->
  unit
val mapi2 :
  (int -> 'a -> 'b -> 'c) -> ('a, 'd) barray -> ('b, 'd) barray ->
  ('c, 'd) barray
val fold_left2 : 
  ('a -> 'b -> 'c -> 'a) -> 'a -> 
  ('b, 'd) barray -> ('c,'d) barray -> 'a
val fold_right2 :
  ('a -> 'b -> 'c -> 'c) -> 
  ('a, 'd) barray -> ('b, 'd) barray -> 
  'c -> 'c
val to_array : ('a, 'd) barray -> 'a array

(*
val of_array : 'a array -> ('a, 'd) barray 
(** throws exception if array isn't of size 'd *)
*)
