open Dim

type ('t, 'd) barray = 't array

let make d x = Array.make (to_int d) x
let init d f = Array.init (to_int d) f
let copy x = Array.copy  x
  (* other array operations go here ... *)
let get = Array.get
let set = Array.set
    
  let unsafe_get : ('a, 'd) barray -> int -> 'a = fun a d ->
    Array.unsafe_get a d

  let unsafe_set : ('a, 'd) barray -> int -> 'a -> unit = fun a d v ->
    Array.unsafe_set a d v

  let combine :
      ('a, 'd) barray -> ('b, 'd) barray -> ('a -> 'b -> 'c) -> ('c,'d) barray =
	fun a b f ->
	  Array.init (Array.length a) (fun i -> f a.(i) b.(i))

  let length : ('a, 'd) barray -> int = fun a -> Array.length a

  let update : ('a, 'd) barray -> int -> 'a -> ('a, 'd) barray =
    fun a d v -> let result = Array.copy a in (Array.set result d v;
result)

  let iter f a = Array.iter f a
  let map f a = Array.map f a
  let iteri f a = Array.iteri f a
  let mapi f a = Array.mapi f a
  let fold_left f x a = Array.fold_left f x a
  let fold_right f a x = Array.fold_right f a x

  let rec iter2 f a1 a2 =
    for i = 0 to length a1 - 1 do
      f (unsafe_get a1 i) (unsafe_get a2 i)
    done

  let rec map2 f a1 a2 =
    let l = length a1 in
    if l = 0 then [||] else
    (let r = Array.make l (f (unsafe_get a1 0) (unsafe_get a2 0)) in
     for i = 1 to l - 1 do
       unsafe_set r i (f (unsafe_get a1 i) (unsafe_get a2 i))
     done;
     r)

  let rec iteri2 f a1 a2 =
    for i = 0 to length a1 - 1 do
      f i (unsafe_get a1 i) (unsafe_get a2 i)
    done

  let mapi2 f a1 a2 =
    let l = length a1 in
    if l = 0 then [||] else
    (let r = Array.make l (f 0 (unsafe_get a1 0) (unsafe_get a2 0)) in
     for i = 1 to l - 1 do
       unsafe_set r i (f i (unsafe_get a1 i) (unsafe_get a2 i))
     done;
     r)

  let fold_left2 f accu a1 a2 =
    let r = ref accu in
    for i = 0 to length a1 - 1 do
      r := f !r (unsafe_get a1 i) (unsafe_get a2 i)
    done;
    !r

  let fold_right2 f a1 a2 accu =
    let r = ref accu in
    for i = length a1 - 1 downto 0 do
      r := f (unsafe_get a1 i) (unsafe_get a2 i) !r
    done;
    !r

  let to_array : ('a, 'd) barray -> 'a array = fun d -> d
(*  let of_array : 'a array -> ('a, 'd) barray = fun d -> d*)
