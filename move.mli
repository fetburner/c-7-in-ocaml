type t

val compare : t -> t -> int

val make : column:int -> row:int -> t
val column : t -> int
val row : t -> int
val to_bits : t -> int64
val of_bits : int64 -> t

val to_string : t -> string
