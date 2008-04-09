type dec
type 'a d0 and 'a d1 and 'a d2 and 'a d3 and 'a d4
type 'a d5 and 'a d6 and 'a d7 and 'a d8 and 'a d9
type zero and nonzero
type ('a, 'z) dim0
type 'a dim = ('a, nonzero) dim0

val dec :            ((dec, zero) dim0 -> 'b) -> 'b

val d0 : 'a dim        -> ('a d0 dim -> 'b) -> 'b
val d1 : ('a, 'z) dim0 -> ('a d1 dim -> 'b) -> 'b
val d2 : ('a, 'z) dim0 -> ('a d2 dim -> 'b) -> 'b
val d3 : ('a, 'z) dim0 -> ('a d3 dim -> 'b) -> 'b
val d4 : ('a, 'z) dim0 -> ('a d4 dim -> 'b) -> 'b
val d5 : ('a, 'z) dim0 -> ('a d5 dim -> 'b) -> 'b
val d6 : ('a, 'z) dim0 -> ('a d6 dim -> 'b) -> 'b
val d7 : ('a, 'z) dim0 -> ('a d7 dim -> 'b) -> 'b
val d8 : ('a, 'z) dim0 -> ('a d8 dim -> 'b) -> 'b
val d9 : ('a, 'z) dim0 -> ('a d9 dim -> 'b) -> 'b

val dim : ('a, 'z) dim0 -> ('a, 'z) dim0

val to_int : ('a, 'z) dim0 -> int
