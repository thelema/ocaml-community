module type OrderedType = sig type t val compare : t -> t -> int end
module type ComparableType =
  sig type t val compare : t -> t -> int val equal : t -> t -> bool end
module type PrintableType = sig type t val to_string : t -> string end
module type HashableType = sig type t val hash : t -> int end
module type SerializableType =
  sig type t val to_string : t -> string val of_string : string -> t end
module type DiscreteType =
  sig
    type t
    val succ : t -> t
    val pred : t -> t
    val min_num : t
    val max_num : t
    val of_int : int -> t
    val to_int : t -> int
    val of_string : string -> t
    val to_string : t -> string
    val compare : t -> t -> int
    val equal : t -> t -> bool
  end
module type NumericType =
  sig
    type t
    val succ : t -> t
    val pred : t -> t
    val min_num : t
    val max_num : t
    val of_int : int -> t
    val to_int : t -> int
    val of_string : string -> t
    val to_string : t -> string
    val compare : t -> t -> int
    val equal : t -> t -> bool
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
val generic_pow :
  'a -> 'a -> ('a -> 'a) -> ('a -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'a -> 'a
module Int :
  sig
    type t = int
    val zero : int
    val one : int
    val neg : int -> int
    val succ : int -> int
    val pred : int -> int
    val abs : int -> int
    val add : int -> int -> int
    val sub : int -> int -> int
    val mul : int -> int -> int
    val div : int -> int -> int
    val modulo : int -> int -> int
    val pow : int -> int -> int
    val min_num : int
    val max_num : int
    val compare : int -> int -> int
    val equal : int -> int -> bool
    val of_int : int -> int
    val to_int : int -> int
    val of_string : string -> int
    val to_string : int -> string
    val hash : int -> int
  end
module Float :
  sig
    type t = float
    val zero : float
    val one : float
    val neg : float -> float
    val succ : float -> float
    val pred : float -> float
    val abs : float -> float
    val add : float -> float -> float
    val sub : float -> float -> float
    val mul : float -> float -> float
    val div : float -> float -> float
    val modulo : float -> float -> float
    val pow : float -> float -> float
    val min_num : float
    val max_num : float
    val compare : 'a -> 'a -> int
    val epsilon : float ref
    val set_precision : float -> unit
    val equal : float -> float -> bool
    val of_int : int -> float
    val to_int : float -> int
    val of_string : string -> float
    val to_string : float -> string
  end
