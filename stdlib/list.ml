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
(*   (C) Flying Frog Consultancy Ltd., 2006                            *)
(* portions lifted from Extlib                                         *)
(* Copyright (C) 2003 Brian Hurt                                       *)
(* Copyright (C) 2003 Nicolas Cannasse                                 *)
(* Copyright 2008 <bluestorm dot dylc on-the-server gmail dot com>     *)
(***********************************************************************)

(* $Id$ *)

(* List operations *)

let rec length_aux len = function
    [] -> len
  | a::l -> length_aux (len + 1) l

let length l = length_aux 0 l

let hd = function
    [] -> failwith "hd"
  | a::l -> a

let tl = function
    [] -> failwith "tl"
  | a::l -> l

let nth l n =
  if n < 0 then invalid_arg "List.nth" else
  let rec nth_aux n = function
    | [] -> failwith "nth"
    | a::t -> if n = 0 then a else nth_aux (n-1) t
  in nth_aux n l


(* FROM EXTLIB *)

(* Thanks to Jacques Garrigue for suggesting the following structure *)
type 'a mut_list =  {
        hd: 'a; 
        mutable tl: 'a list
}
external inj : 'a mut_list -> 'a list = "%identity"

external magic : unit -> 'b = "%identity"

let dummy_node () = { hd = magic (); tl = [] }

let append l1 l2 =  (* EXTLIB *) 
  match l1 with
    | [] -> l2
    | h :: t ->
        let rec loop dst = function
          | [] ->
              dst.tl <- l2
          | h :: t ->
              let cell = { hd = h; tl = [] } in
              dst.tl <- inj cell;
              loop cell t
        in
        let r = { hd = h; tl = [] } in
        loop r t;
        inj r

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l (a :: l2)

let rev l = rev_append l []

let rec flatten l = 
  let rec inner dst = function
    | [] -> dst
    | h :: t ->
        let r = { hd = h; tl = [] } in
        dst.tl <- inj r;
        inner r t
  in
  let rec outer dst = function
    | [] -> ()
    | h :: t -> outer (inner dst h) t
  in
  let r = dummy_node () in
  outer r l;
  r.tl

let concat = flatten

let map f = function
  | [] -> []
  | h :: t ->
      let rec loop dst = function
        | [] -> ()
        | h :: t ->
            let r = { hd = f h; tl = [] } in
            dst.tl <- inj r;
            loop r t
      in
      let r = { hd = f h; tl = [] } in
      loop r t;
      inj r

let mapi f = function
  | [] -> []
  | h :: t ->
      let rec loop n dst = function
        | [] -> ()
        | h :: t ->
            let r = { hd = f n h; tl = [] } in
            dst.tl <- inj r;
            loop (n+1) r t
      in
      let r = { hd = f 0 h; tl = [] } in
      loop 1 r t;
      inj r

let rev_map f l =
  let rec rmap_f accu = function
    | [] -> accu
    | a::l -> rmap_f (f a :: accu) l
  in
  rmap_f [] l
;;

let rec iter f = function
    [] -> ()
  | a::l -> f a; iter f l

let rec fold_left f accu l =
  match l with
    [] -> accu
  | a::l -> fold_left f (f accu a) l

let fold_right_max = 1000
let fold_right_chunk_size = 500

let fold_right f li init =
  let rec fold_chunk li =
    let (n, init) = jump 0 li in
    partial_fold init li n
  and jump n = function
    | [] -> (n, init)
    | _::tl when n < fold_right_chunk_size -> jump (n + 1) tl
    | li -> (n, fold_chunk li)
  and partial_fold partial_init li = function
    | 0 -> partial_init
    | n -> match li with
        | [] -> assert false
        | hd::tl -> f hd (partial_fold partial_init tl (n -1))  in
  let rec loop n = function
    | [] -> init
    | h :: t when n < fold_right_max -> f h (loop (n+1) t)
    | li -> fold_chunk li
  in loop 0 li
       
let fold_right f l init =
        let rec tail_loop acc = function
                | [] -> acc
                | h :: t -> tail_loop (f h acc) t
        in
        let rec loop n = function
                | [] -> init
                | h :: t ->
                        if n < fold_right_max then
                                f h (loop (n+1) t)
                        else
                                f h (tail_loop init (rev t))
        in
        loop 0 l

let map2 f l1 l2 =
        let rec loop dst src1 src2 =
                match src1, src2 with
                        | [], [] -> ()
                        | h1 :: t1, h2 :: t2 ->
                                let r = { hd = f h1 h2; tl = [] } in
                                dst.tl <- inj r;
                                loop r t1 t2
                        | _ -> invalid_arg "List.map2"
        in
        let dummy = dummy_node () in
        loop dummy l1 l2;
        dummy.tl


let rev_map2 f l1 l2 =
  let rec rmap2_f accu l1 l2 =
    match (l1, l2) with
    | ([], []) -> accu
    | (a1::l1, a2::l2) -> rmap2_f (f a1 a2 :: accu) l1 l2
    | (_, _) -> invalid_arg "List.rev_map2"
  in
  rmap2_f [] l1 l2
;;

let rec iter2 f l1 l2 =
  match (l1, l2) with
    ([], []) -> ()
  | (a1::l1, a2::l2) -> f a1 a2; iter2 f l1 l2
  | (_, _) -> invalid_arg "List.iter2"

let rec fold_left2 f accu l1 l2 =
  match (l1, l2) with
    ([], []) -> accu
  | (a1::l1, a2::l2) -> fold_left2 f (f accu a1 a2) l1 l2
  | (_, _) -> invalid_arg "List.fold_left2"

let fold_right2 f l1 l2 init =
        let rec tail_loop acc l1 l2 =
                match l1, l2 with
                | [] , [] -> acc
                | h1 :: t1 , h2 :: t2 -> tail_loop (f h1 h2 acc) t1 t2
                | _ -> invalid_arg "List.fold_right2"
        in
        let rec loop n l1 l2 =
                match l1, l2 with
                | [], [] -> init
                | h1 :: t1, h2 :: t2 ->
                        if n < fold_right_max then
                                f h1 h2 (loop (n+1) t1 t2)
                        else
                                f h1 h2 (tail_loop init (rev t1) (rev t2))
                | _ -> invalid_arg "List.fold_right2"
        in
        loop 0 l1 l2

let rec for_all p = function
    [] -> true
  | a::l -> p a && for_all p l

let rec exists p = function
    [] -> false
  | a::l -> p a || exists p l

let rec for_all2 p l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (a1::l1, a2::l2) -> p a1 a2 && for_all2 p l1 l2
  | (_, _) -> invalid_arg "List.for_all2"

let rec exists2 p l1 l2 =
  match (l1, l2) with
    ([], []) -> false
  | (a1::l1, a2::l2) -> p a1 a2 || exists2 p l1 l2
  | (_, _) -> invalid_arg "List.exists2"

let rec mem x = function
    [] -> false
  | a::l -> compare a x = 0 || mem x l

let rec memq x = function
    [] -> false
  | a::l -> a == x || memq x l

let rec assoc x = function
    [] -> raise Not_found
  | (a,b)::l -> if compare a x = 0 then b else assoc x l

let rec assq x = function
    [] -> raise Not_found
  | (a,b)::l -> if a == x then b else assq x l

let rec mem_assoc x = function
  | [] -> false
  | (a, b) :: l -> compare a x = 0 || mem_assoc x l

let rec mem_assq x = function
  | [] -> false
  | (a, b) :: l -> a == x || mem_assq x l

let remove_assoc x lst = 
        let rec loop dst = function
                | [] -> ()
                | (a, _ as pair) :: t ->
                        if a = x then
                                dst.tl <- t
                        else
                                let r = { hd = pair; tl = [] } in
                                dst.tl <- inj r;
                                loop r t
        in
        let dummy = dummy_node () in
        loop dummy lst;
        dummy.tl

let remove_assq x lst = 
        let rec loop dst = function
                | [] -> ()
                | (a, _ as pair) :: t ->
                        if a == x then
                                dst.tl <- t
                        else
                                let r = { hd =  pair; tl = [] } in
                                dst.tl <- inj r;
                                loop r t
        in
        let dummy = dummy_node() in
        loop dummy lst;
        dummy.tl

let rec find p = function
  | [] -> raise Not_found
  | x :: l -> if p x then x else find p l

let find_all p l = 
        let rec findnext dst = function
                | [] -> ()
                | h :: t -> 
                        if p h then
                                let r = { hd = h; tl = [] } in
                                dst.tl <- inj r;
                                findnext r t
                        else
                                findnext dst t
        in
        let dummy = dummy_node () in
        findnext dummy l;
        dummy.tl

let filter = find_all

let partition p lst = 
  let rec loop yesdst nodst = function
    | [] -> ()
    | h :: t ->
        let r = { hd = h; tl = [] } in
        if p h then
          begin
            yesdst.tl <- inj r;
            loop r nodst t
          end
        else
          begin
            nodst.tl <- inj r;
            loop yesdst r t
          end
  in
  let yesdummy = dummy_node()
  and nodummy = dummy_node()
  in
  loop yesdummy nodummy lst;
  yesdummy.tl, nodummy.tl

let split lst =
        let rec loop adst bdst = function
                | [] -> ()
                | (a, b) :: t -> 
                        let x = { hd = a; tl = [] } 
                        and y = { hd = b; tl = [] } in
                        adst.tl <- inj x;
                        bdst.tl <- inj y;
                        loop x y t
        in
        let adummy = dummy_node ()
        and bdummy = dummy_node ()
        in
        loop adummy bdummy lst;
        adummy.tl, bdummy.tl

let combine l1 l2 =
        let rec loop dst l1 l2 =
                match l1, l2 with
                | [], [] -> ()
                | h1 :: t1, h2 :: t2 -> 
                        let r = { hd = h1, h2; tl = [] } in
                        dst.tl <- inj r;
                        loop r t1 t2
                | _, _ -> invalid_arg "List.combine"
        in
        let dummy = dummy_node () in
        loop dummy l1 l2;
        dummy.tl

(** sorting *)

let rec merge cmp l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
      if cmp h1 h2 <= 0
      then h1 :: merge cmp t1 l2
      else h2 :: merge cmp l1 t2
;;

let rec chop k l =
  if k = 0 then l else begin
    match l with
    | x::t -> chop (k-1) t
    | _ -> assert false
  end
;;

let stable_sort cmp l =
  let rec rev_merge l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1::t1, h2::t2 ->
        if cmp h1 h2 <= 0
        then rev_merge t1 l2 (h1::accu)
        else rev_merge l1 t2 (h2::accu)
  in
  let rec rev_merge_rev l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1::t1, h2::t2 ->
        if cmp h1 h2 > 0
        then rev_merge_rev t1 l2 (h1::accu)
        else rev_merge_rev l1 t2 (h2::accu)
  in
  let rec sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
       if cmp x1 x2 <= 0 then [x1; x2] else [x2; x1]
    | 3, x1 :: x2 :: x3 :: _ ->
       if cmp x1 x2 <= 0 then begin
         if cmp x2 x3 <= 0 then [x1; x2; x3]
         else if cmp x1 x3 <= 0 then [x1; x3; x2]
         else [x3; x1; x2]
       end else begin
         if cmp x1 x3 <= 0 then [x2; x1; x3]
         else if cmp x2 x3 <= 0 then [x2; x3; x1]
         else [x3; x2; x1]
       end
    | n, l ->
       let n1 = n asr 1 in
       let n2 = n - n1 in
       let l2 = chop n1 l in
       let s1 = rev_sort n1 l in
       let s2 = rev_sort n2 l2 in
       rev_merge_rev s1 s2 []
  and rev_sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
       if cmp x1 x2 > 0 then [x1; x2] else [x2; x1]
    | 3, x1 :: x2 :: x3 :: _ ->
       if cmp x1 x2 > 0 then begin
         if cmp x2 x3 > 0 then [x1; x2; x3]
         else if cmp x1 x3 > 0 then [x1; x3; x2]
         else [x3; x1; x2]
       end else begin
         if cmp x1 x3 > 0 then [x2; x1; x3]
         else if cmp x2 x3 > 0 then [x2; x3; x1]
         else [x3; x2; x1]
       end
    | n, l ->
       let n1 = n asr 1 in
       let n2 = n - n1 in
       let l2 = chop n1 l in
       let s1 = sort n1 l in
       let s2 = sort n2 l2 in
       rev_merge s1 s2 []
  in
  let len = length l in
  if len < 2 then l else sort len l
;;

let sort = stable_sort;;
let fast_sort = stable_sort;;

(* Note: on a list of length between about 100000 (depending on the minor
   heap size and the type of the list) and Sys.max_array_size, it is
   actually faster to use the following, but it might also use more memory
   because the argument list cannot be deallocated incrementally.

   Also, there seems to be a bug in this code or in the
   implementation of obj_truncate.

external obj_truncate : 'a array -> int -> unit = "caml_obj_truncate"

let array_to_list_in_place a =
  let l = Array.length a in
  let rec loop accu n p =
    if p <= 0 then accu else begin
      if p = n then begin
        obj_truncate a p;
        loop (a.(p-1) :: accu) (n-1000) (p-1)
      end else begin
        loop (a.(p-1) :: accu) n (p-1)
      end
    end
  in
  loop [] (l-1000) l
;;

let stable_sort cmp l =
  let a = Array.of_list l in
  Array.stable_sort cmp a;
  array_to_list_in_place a
;;
*)


(****** FUNCTIONS FROM STDLIB2 ********)
let cons h t = h :: t

let rec fold_left f accu = function
  | h1::h2::h3::h4::t ->
      fold_left f (f (f (f (f accu h1) h2) h3) h4) t
  | h::t -> fold_left f (f accu h) t
  | [] -> accu


(********************************
  This implementation of fold_right causes 'make world' to fail with error linking dynlink.cma:

  ../../boot/ocamlrun ../../ocamlc -warn-error A -I ../../stdlib -I ../../utils -I ../../typing -I ../../bytecomp -o extract_crc dynlink.cma extract_crc.cmo
  Error while linking dynlink.cma(Dynlink):
  Reference to undefined global `Dynlinkaux'


let rec fold_right_aux n f list accu =
  if n<=0 then Array.fold_right f (Array.of_list list) accu else
    match list with
      | h1::h2::h3::h4::t ->
	  f h1 (f h2 (f h3 (f h4 (fold_right_aux (n - 4) f t accu))))
      | h::t -> fold_right_aux (n - 1) f t (f h accu)
      | [] -> accu

let fold_right f list accu = fold_right_aux max_int f list accu
let rec rev_iter f = function
    [] -> ()
  | h::t ->
      rev_iter f t;
      f h
****************************************)

(* DOCUMENT THIS - sometimes slower than simple implementation?*)
let rec map_k f t k = match t with
  | [] -> k []
  | h::t -> map_k f t (fun t -> k(f h :: t))

let rec map_aux n f = function
    [] -> []
  | h :: t ->
      let h = f h in
      if n=0 then map_k f t (fun t -> h :: t) else h :: map_aux (n-1) f t

let map f l = map_aux 1024 f l

let count pred l =
  fold_left (fun count e -> count + if pred e then 1 else 0) 0 l

let positions pred l =
  let aux (i, is) e = i + 1, if pred e then i :: is else is in
  rev (snd (fold_left aux (0, []) l))

let split_nth index = function
  | [] -> if index = 0 then [],[] else invalid_arg "List.split_nth"
  | (h :: t as l) ->
      if index = 0 then [],l
      else if index < 0 then invalid_arg "List.split_nth"
      else
        let rec loop n dst l =
          if n = 0 then l else
            match l with
              | [] -> invalid_arg "List.split_nth"
              | h :: t ->
                  let r = { hd =  h; tl = [] } in
                  dst.tl <- inj r;
                  loop (n-1) r t 
        in
        let r = { hd = h; tl = [] } in
        inj r, loop (index-1) r t
	  
let chop2 = split_nth 

(***** EXTLIB ********)

let take n l =
        let rec loop n dst = function
                | h :: t when n > 0 ->
                        let r = { hd = h; tl = [] } in
                        dst.tl <- inj r;
                        loop (n-1) r t
                | _ ->
                        ()
        in
        let dummy = dummy_node() in
        loop n dummy l;
        dummy.tl

let drop = chop

(* takewhile and dropwhile by Richard W.M. Jones. *)
let rec takewhile f = function
  | [] -> []
  | x :: xs when f x -> x :: takewhile f xs
  | _ -> []

let rec dropwhile f = function
  | [] -> []
  | x :: xs when f x -> dropwhile f xs
  | xs -> xs

let reduce f l = fold_left f (hd l) (tl l)

let make i x =
  if i < 0 then invalid_arg "List.make";
  let rec make' x = function
    | 0 -> []
    | i -> x :: make' x (i-1)
  in
  make' x i

(* FIXME: USE A HASHTABLE? *)
let rec unique ?(cmp = ( = )) l =
  let rec loop dst = function
    | [] -> ()
    | h :: t ->
        match exists (cmp h) t with
          | true -> loop dst t
          | false ->
              let r = { hd =  h; tl = [] }  in
              dst.tl <- inj r;
              loop r t
  in
  let dummy = dummy_node() in
  loop dummy l;
  dummy.tl
    

let filter_map f l =
  let rec loop dst = function
    | [] -> ()
    | h :: t ->
        match f h with
          | None -> loop dst t
          | Some x ->
              let r = { hd = x; tl = [] }  in
              dst.tl <- inj r;
              loop r t
  in
  let dummy = dummy_node() in
  loop dummy l;
  dummy.tl

let rfind p l = find p (rev l)

let rec findi p l =
  let rec loop n = function
    | [] -> raise Not_found
    | h :: t ->
        if p n h then (n,h) else loop (n+1) t
  in
  loop 0 l

let find_exc f e l =
  try
    find f l
  with
      Not_found -> raise e

let rec init size f =
  if size = 0 then [] 
  else if size < 0 then invalid_arg "List.init"
  else
    let rec loop dst n =
      if n < size then
        let r = { hd = f n; tl = [] } in
        dst.tl <- inj r;
        loop r (n+1)
    in
    let r = { hd = f 0; tl = [] } in
    loop r 1;
    inj r
      
let iteri f l = 
  let rec loop n = function
    | [] -> ()
    | h :: t ->
        f n h;
        loop (n+1) t
  in
  loop 0 l
    
let first = hd

let rec last = function
  | [] -> invalid_arg "List.last"
  | h :: [] -> h
  | _ :: t -> last t

(* in pervasives?  3 List.-- 5 doesn't have the ring that 3--5 has.  Also, right endpoint version?  --]?  *)
let rec (--) = fun m n -> if m >= n then [] else m::((m + 1) -- n)

let unfold f seed = 
  match f seed with
      None -> []
    | Some (s', e) ->
	let rec loop dst s = 
	  match f s with
	      None -> ()
	    | Some (s',e) -> 
		let r = { hd = e; tl = [] } in
		dst.tl <- inj r;
		loop r s'
	in
	let r = { hd = e; tl = [] } in
	loop r s';
	inj r
