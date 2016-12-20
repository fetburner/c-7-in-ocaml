type t = Black | White

let to_char = function
  | Black -> 'X'
  | White -> 'O'

let of_char = function
  | 'X' -> Black
  | 'O' -> White
  | _ -> raise (Invalid_argument "Color.of_char")

let flip = function
  | Black -> White
  | White -> Black
