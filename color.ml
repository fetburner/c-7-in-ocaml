type t = Black | White

let to_char : t -> char = function
  | Black -> 'X'
  | White -> 'O'

let of_char : char -> t = function
  | 'X' -> Black
  | 'O' -> White
  | _ -> raise (Invalid_argument "Color.of_char")

let flip : t -> t = function
  | Black -> White
  | White -> Black
