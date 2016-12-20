type t

val compare : t -> t -> int

(* make = fun column row -> ... *)
val make : int -> int -> t
val column : t -> int
val row : t -> int
val to_bits : t -> int64
val of_bits : int64 -> t

val to_string : t -> string
