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
(*   (C) 2008 Edgar Friendly <thelema314@gmail.com>                    *)
(***********************************************************************)

(* $Id$ *)

(* String operations *)

external length : string -> int = "%string_length"
external get : string -> int -> char = "%string_safe_get"
external set : string -> int -> char -> unit = "%string_safe_set"
external create : int -> string = "caml_create_string"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit : string -> int -> string -> int -> int -> unit
                     = "caml_blit_string" "noalloc"
external unsafe_fill : string -> int -> int -> char -> unit
                     = "caml_fill_string" "noalloc"

let make n c =
  let s = create n in
  unsafe_fill s 0 n c;
  s

let copy s =
  let len = length s in
  let r = create len in
  unsafe_blit s 0 r 0 len;
  r

let sub s ofs len =
  if ofs < 0 || len < 0 || ofs > length s - len
  then invalid_arg "String.sub"
  else begin
    let r = create len in
    unsafe_blit s ofs r 0 len;
    r
  end

let fill s ofs len c =
  if ofs < 0 || len < 0 || ofs > length s - len
  then invalid_arg "String.fill"
  else unsafe_fill s ofs len c

let blit s1 ofs1 s2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > length s1 - len
             || ofs2 < 0 || ofs2 > length s2 - len
  then invalid_arg "String.blit"
  else unsafe_blit s1 ofs1 s2 ofs2 len

let iter f a =
  for i = 0 to length a - 1 do f(unsafe_get a i) done

let concat sep l =
  match l with
    [] -> ""
  | hd :: tl ->
      let num = ref 0 and len = ref 0 in
      List.iter (fun s -> incr num; len := !len + length s) l;
      let r = create (!len + length sep * (!num - 1)) in
      unsafe_blit hd 0 r 0 (length hd);
      let pos = ref(length hd) in
      List.iter
        (fun s ->
          unsafe_blit sep 0 r !pos (length sep);
          pos := !pos + length sep;
          unsafe_blit s 0 r !pos (length s);
          pos := !pos + length s)
        tl;
      r

let of_list string_of l =
  "["^concat "; " (List.map string_of l)^"]"


external is_printable: char -> bool = "caml_is_printable"
external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

let escaped s =
  let n = ref 0 in
    for i = 0 to length s - 1 do
      n := !n +
        (match unsafe_get s i with
           '"' | '\\' | '\n' | '\t' -> 2
          | c -> if is_printable c then 1 else 4)
    done;
    if !n = length s then s else begin
      let s' = create !n in
        n := 0;
        for i = 0 to length s - 1 do
          begin
            match unsafe_get s i with
              ('"' | '\\') as c ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
            | '\n' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
            | '\t' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 't'
            | c ->
                if is_printable c then
                  unsafe_set s' !n c
                else begin
                  let a = char_code c in
                  unsafe_set s' !n '\\';
                  incr n;
                  unsafe_set s' !n (char_chr (48 + a / 100));
                  incr n;
                  unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
                  incr n;
                  unsafe_set s' !n (char_chr (48 + a mod 10))
                end
          end;
          incr n
        done;
        s'
      end

let map f s =
  let l = length s in
  if l = 0 then s else begin
    let r = create l in
    for i = 0 to l - 1 do unsafe_set r i (f(unsafe_get s i)) done;
    r
  end

let uppercase s = map Char.uppercase s
let lowercase s = map Char.lowercase s

let apply1 f s =
  if length s = 0 then s else begin
    let r = copy s in
    unsafe_set r 0 (f(unsafe_get s 0));
    r
  end

let capitalize s = apply1 Char.uppercase s
let uncapitalize s = apply1 Char.lowercase s

let rec index_rec s lim i c =
  if i >= lim then raise Not_found else
  if unsafe_get s i = c then i else index_rec s lim (i+1) c;;

let index s c = index_rec s (length s) 0 c;;

let index_from s i c =
  if i < 0 || i > length s then invalid_arg "String.index_from" else
  index_rec s (length s) i c;;

let rec rindex_rec s i c =
  if i < 0 then raise Not_found else
  if unsafe_get s i = c then i else rindex_rec s (i-1) c;;

let rindex s c = rindex_rec s (length s - 1) c;;

let rindex_from s i c =
  if i < -1 || i >= length s then invalid_arg "String.rindex_from" else
  rindex_rec s i c;;

let contains_from s i c =
  if i < 0 || i > length s then invalid_arg "String.contains_from" else
  try ignore(index_rec s (length s) i c); true with Not_found -> false;;

let rcontains_from s i c =
  if i < 0 || i >= length s then invalid_arg "String.rcontains_from" else
  try ignore(rindex_rec s i c); true with Not_found -> false;;

let contains s c = contains_from s 0 c;;

type t = string

let compare (x: t) (y: t) = Pervasives.compare x y

(*FROM STDLIB2*)
(* String.create is a source of non-determinism in the OCaml stdlib. *)
(*let create n = make n '\000' (* INCLUDE THIS? *) *)

let init n f =
  let s = make n (f 0) in
  for i=1 to n-1 do
    unsafe_set s i (f i);
  done;
  s

let map f s = init (length s) (fun i -> f (unsafe_get s i))
  
let rev_map f s =
  let n = length s in
  init n (fun i -> f (unsafe_get s (n - i - 1)))

let rev_iter f s =
  for i = length s - 1 downto 0 do
    f (unsafe_get s i)
  done

let fold_left f accu s =
  let accu = ref accu in
  for i = 0 to length s - 1 do
    accu := f !accu (unsafe_get s i)
  done;
  !accu

let fold_right f s accu =
  let accu = ref accu in
  for i = length s - 1 downto 0 do
    accu := f (unsafe_get s i) !accu
  done;
  !accu

let explode string =
  fold_right List.cons string []

let implode list =
  concat "" (List.map (make 1) list) (* make more efficient? *)

let splice s1 off len s2 = 
  let len1 = length s1 and len2 = length s2 in
  let out_len = len1 - len + len2 in
  let s = create out_len in
  blit s1 0 s 0 off; (* s1 before splice point *)
  blit s2 0 s off len2; (* s2 at splice point *)
  blit s1 (off+len) s (off+len2) (len1 - (off+len)); (* s1 after off+len *)
  s

(* FROM EXTLIB *)
let starts_with str p =
  let len = length p in
  if length str < len then 
    false
  else
    sub str 0 len = p (* TODO: make efficient? - remove allocation/copy *)
      
let ends_with s e =
  let el = length e in
  let sl = length s in
  if sl < el then
    false
  else
    sub s (sl-el) el = e (* TODO: make efficient? - remove allocation/copy *)
      
let find str sub =
  let sublen = length sub in
  if sublen = 0 then
    0
  else
    let found = ref 0 in
    let len = length str in
    try
      for i = 0 to len - sublen do
        let j = ref 0 in
        while unsafe_get str (i + !j) = unsafe_get sub !j do
          incr j;
          if !j = sublen then begin found := i; raise Exit; end;
        done;
      done;
      failwith "String.find"
    with
        Exit -> !found

let exists str sub =
  try
    ignore(find str sub);
    true
  with
      Failure "String.find" -> false
	
let strip ?(chars=" \t\r\n") s =
  let p = ref 0 in
  let l = length s in
  while !p < l && contains chars (unsafe_get s !p) do
    incr p;
  done;
  let p = !p in
  let l = ref (l - 1) in
  while !l >= p && contains chars (unsafe_get s !l) do
    decr l;
  done;
  sub s p (!l - p + 1)
	  
let split str sep =
  let p = find str sep in
  let len = length sep in
  let slen = length str in
  sub str 0 p, sub str (p + len) (slen - p - len)
    
let nsplit str sep = (* TODO: optimize allocations - too many re-allocations *)
  if str = "" then []
  else (
    let rec nsplit str sep =
      try
        let s1 , s2 = split str sep in
        s1 :: nsplit s2 sep
      with
          Failure "String.find" -> [str]
    in
    nsplit str sep
  )
    
let join = concat
  
let slice ?(first=0) ?(last=max_int) s =
  let clip _min _max x = max _min (min _max x) in
  let i = clip 0 (length s)
    (if (first<0) then (length s) + first else first)
  and j = clip 0 (length s)
    (if (last<0) then (length s) + last else last)
  in
  if i>=j || i=length s then
    create 0
  else
    sub s i (j-i)
      
let lchop s =
  if s = "" then "" else sub s 1 (length s - 1)
    
let rchop s =
  if s = "" then "" else sub s 0 (length s - 1)

let chomp ?(char='\n') s = 
  if s = "" then "" else
    let l = length s in
    if get s (l-1) = char then sub s 0 (l-1)
    else s
    
let of_int = string_of_int
  
let of_float = string_of_float
  
let of_char = make 1
  
let to_int = int_of_string
	
let to_float = float_of_string

let replace_chars f s =
  let len = length s in
  let tlen = ref 0 in
  let rec loop i acc =
    if i = len then
      acc
    else 
      let s = f (unsafe_get s i) in
      tlen := !tlen + length s;
      loop (i+1) (s :: acc)
  in
  let strs = loop 0 [] in
  let sbuf = create !tlen in
  let pos = ref !tlen in
  let rec loop2 = function
    | [] -> ()
    | s :: acc ->
        let len = length s in
        pos := !pos - len;
        blit s 0 sbuf !pos len;
        loop2 acc
  in
  loop2 strs;
  sbuf

let replace ~str ~sub ~by =
  try
    let i = find str sub in
    (true, (slice ~last:i str) ^ by ^ 
       (slice ~first:(i+(length sub)) str))
  with
      Failure "String.find" -> (false, copy str)

let to_enum s =
        let l = length s in
        let rec make i =
                Enum.make
                ~next:(fun () ->
                        if !i = l then
                                raise Enum.No_more_elements
                        else
                                let p = !i in
                                incr i;
                                unsafe_get s p
                        )
                ~count:(fun () -> l - !i)
                ~clone:(fun () -> make (ref !i))
        in
        make (ref 0)

let of_enum e =
        let l = Enum.count e in
        let s = create l in
        let i = ref 0 in
        Enum.iter (fun c -> unsafe_set s !i c; incr i) e;
        s
