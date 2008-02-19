(***********************************************************************)
(*                                                                     *)
(*              (community)  Objective Caml                            *)
(*                                                                     *)
(*            Edgar Friendly <thelema314@gmail.com>                    *)
(*                                                                     *)
(* Copyright 2008 Edgar Friendly.                                      *)
(* Copyright 2008 <bluestorm dot dylc on-the-server gmail dot com>     *)
(*                   All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

module type OrderedType =
sig
  type t
  val compare: t -> t -> int
end

module type ComparableType = 
sig
  type t
  val compare: t -> t -> int
  val equal : t -> t -> bool
end

module type PrintableType = 
sig
  type t
  val to_string : t -> string
end

module type SerializableType = 
sig
  type t
  val to_string : t -> string
  val of_string : string -> t
end

module type DiscreteType = sig
  type t
  val succ : t -> t
  val pred : t -> t

  val min_num : t
  val max_num : t

  val of_int : int -> t
  val to_int : t -> int (* may raise error if not with int range *)
    
  val of_string : string -> t
  val to_string : t -> string

  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module type NumericType = sig
  include DiscreteType

  val zero : t
  val one : t

  val neg : t -> t
  val abs : t -> t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t

  val modulo : t -> t -> t
  val pow : t -> t -> t
end

let generic_pow zero one div_two mod_two ( * ) =
  let rec pow a n =
    if n = zero then one
    else if n = one then a
    else
      let b = pow a (div_two n) in
      b * b * (if mod_two n = zero then one else a)
  in pow

module Int = struct
  type t = int
  
  let zero, one = 0, 1

  let neg = (~-)
  let succ, pred, abs = succ, pred, abs

  let add, sub, mul, div = (+), (-), ( * ), (/)

  let modulo a b = a mod b
  let pow = generic_pow 0 1 (fun n -> n asr 1) (fun n -> n land 1) ( * )

  let min_num, max_num = min_int, max_int
  let compare = (-)
  let equal (a:int) b = a = b

  let of_int (n:int) = n
  let to_int (n:int) = n
  let of_string = int_of_string
  let to_string = string_of_int
end

module Float = struct
  type t = float
  let zero, one = 0., 1.
  let neg = (~-.)

  let succ x = x +. 1.
  let pred x = x -. 1.
  let abs = abs_float

  let add, sub, mul, div = (+.), (-.), ( *.), (/.)
  let modulo = mod_float
  let pow = ( ** )

  let min_num, max_num = neg_infinity, infinity
  let compare = compare
  let epsilon = ref 0.00001
  let set_precision e = epsilon := e
  let equal a b = abs(b-.a) < !epsilon
    
  let of_int = float_of_int
  let to_int = int_of_float

  let of_string = float_of_string
  let to_string = string_of_float
end
