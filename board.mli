type t

val compare : t -> t -> int
(* serializer *)
val to_string : Color.t -> t -> string
(* deserializer *)
val of_string : Color.t -> string -> t

val flip : t -> t

val count_disks : t -> int
val count_legal_moves : t -> int
val eval : (int64 * int) list-> t -> int

val legal_moves : t -> Move.t list
val perform_move : Move.t -> t -> t

