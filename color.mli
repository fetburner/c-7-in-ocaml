type t = Black | White

(* serializer *)
val to_char : t -> char
(* deserializer *)
val of_char : char -> t

val flip : t -> t
