type dec = unit
type 'a d0 = unit
type 'a d1 = unit
type 'a d2 = unit
type 'a d3 = unit
type 'a d4 = unit
type 'a d5 = unit
type 'a d6 = unit
type 'a d7 = unit
type 'a d8 = unit
type 'a d9 = unit
type zero = unit
type nonzero = unit

type ('a, 'z) dim0 = int (* Phantom type *)
type 'a dim = ('a, nonzero) dim0

let dec k = k 0

let d0 d k = k (10 * d + 0)
let d1 d k = k (10 * d + 1)
let d2 d k = k (10 * d + 2)
let d3 d k = k (10 * d + 3)
let d4 d k = k (10 * d + 4)
let d5 d k = k (10 * d + 5)
let d6 d k = k (10 * d + 6)
let d7 d k = k (10 * d + 7)
let d8 d k = k (10 * d + 8)
let d9 d k = k (10 * d + 9)

let dim d = d

let to_int d = d
