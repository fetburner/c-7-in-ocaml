external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

type t = int64

let compare = Int64.compare

let make column row =
  if
    column < 0 || 8 <= column ||
    row < 0 || 8 <= row
  then raise (Invalid_argument "Move.make")
  else Int64.shift_left 1L (8 * row + column)

let column m =
  List.map (fun (mask, weight) ->
    if Int64.logand mask m = 0L then weight
    else 0)
    [ (0x0f0f0f0f0f0f0f0fL, 4);
      (0x3333333333333333L, 2);
      (0x5555555555555555L, 1) ]
  |> List.fold_left ( + ) 0
let row m =
  List.map (fun (mask, weight) ->
    if Int64.logand mask m = 0L then weight
    else 0)
    [ (0x00000000ffffffffL, 4);
      (0x0000ffff0000ffffL, 2);
      (0x00ff00ff00ff00ffL, 1) ]
  |> List.fold_left ( + ) 0

let to_bits m = m
let of_bits m = m

let to_string m =
  String.make 1 (Char.chr (Char.code 'A' + column m)) ^
  String.make 1 (Char.chr (Char.code '1' + row m))
