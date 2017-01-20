external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

type t =
  { my_board : int64;
    enemy_board : int64 }

let compare : t -> t -> int = fun b1 b2 ->
  match Int64.compare b1.my_board b2.my_board with
  | 0 -> Int64.compare b1.enemy_board b2.enemy_board
  | n -> n

(* serializer *)
let to_string : Color.t -> t -> string = fun c b ->
  let (white_board, black_board) =
    match c with
    | Color.White -> (b.my_board, b.enemy_board)
    | Color.Black -> (b.enemy_board, b.my_board) in
  Array.init 64 (fun x ->
    match
      Int64.logand white_board (Int64.shift_left 1L x) <> 0L,
      Int64.logand black_board (Int64.shift_left 1L x) <> 0L
    with
    | true, false -> "O"
    | false, true -> "X"
    | false, false -> "-"
    | _, _ -> raise (Invalid_argument "Board.to_string"))
  |> Array.fold_left ( ^ ) ""

(* deserializer *)
let of_string : Color.t -> string -> t = fun c s ->
  if String.length s <> 64 then
    raise (Invalid_argument "Board.of_string")
  else
    Array.init 64 (fun x ->
      match s.[x] with
      | 'O' ->
          (Int64.shift_left 1L x, 0L)
      | 'X' ->
          (0L, Int64.shift_left 1L x)
      | '-' ->
          (0L, 0L)
      | _ ->
          raise (Invalid_argument "Board.of_string"))
    |> Array.fold_left (fun (b11, b12) (b21, b22) ->
        (Int64.logor b11 b21, Int64.logor b12 b22)) (0L, 0L)
    |> (fun (b1, b2) ->
        match c with
        | Color.White -> { my_board = b1; enemy_board = b2 }
        | Color.Black -> { my_board = b2; enemy_board = b1 })

let flip : t -> t = fun b ->
  { my_board = b.enemy_board; enemy_board = b.my_board }

let population_count : int64 -> int = fun bits ->
  Int64.to_int (List.fold_left (fun bits (mask, shamt) ->
    Int64.add (Int64.logand mask bits) (Int64.logand mask (Int64.shift_right_logical bits shamt))) bits
    [ (0x5555555555555555L, 1);
      (0x3333333333333333L, 2);
      (0x0f0f0f0f0f0f0f0fL, 4);
      (0x00ff00ff00ff00ffL, 8);
      (0x0000ffff0000ffffL, 16);
      (0x00000000ffffffffL, 32) ])

let count_disks : t -> int = fun b ->
  population_count (Int64.logor b.my_board b.enemy_board)

let eval : (int64 * int) list -> t -> int = fun weights b ->
  List.fold_left ( + ) 0
    (List.map (fun (bits, weight) ->
      weight
      * (population_count (Int64.logand bits b.my_board)
          - population_count (Int64.logand bits b.enemy_board))) weights)

let legal_moves_aux : t -> int64 = fun b ->
  List.map (fun (mask, shifter) ->
    let masked_enemy = Int64.logand b.enemy_board mask in
    Array.make 5 0
    |> Array.fold_left (fun t _ ->
        Int64.logor t (Int64.logand masked_enemy (shifter t))) (Int64.logand masked_enemy (shifter b.my_board))
    |> shifter)
    [ (* right *)
      (0x7e7e7e7e7e7e7e7eL, fun t -> Int64.shift_left t 1);
      (* left *)
      (0x7e7e7e7e7e7e7e7eL, fun t -> Int64.shift_right_logical t 1);
      (* upto *)
      (0x00ffffffffffff00L, fun t -> Int64.shift_left t 8);
      (* downto *)
      (0x00ffffffffffff00L, fun t -> Int64.shift_right_logical t 8);
      (* upper right *)
      (0x007e7e7e7e7e7e00L, fun t -> Int64.shift_left t 7);
      (* upper left *)
      (0x007e7e7e7e7e7e00L, fun t -> Int64.shift_left t 9);
      (* lower right *)
      (0x007e7e7e7e7e7e00L, fun t -> Int64.shift_right_logical t 9);
      (* upper right *)
      (0x007e7e7e7e7e7e00L, fun t -> Int64.shift_right_logical t 7) ]
  |> List.fold_left Int64.logor 0L
  |> Int64.logand (Int64.lognot b.my_board)
  |> Int64.logand (Int64.lognot b.enemy_board)

let legal_moves : t -> Move.t list = fun b ->
  let bits = legal_moves_aux b in
  List.fold_left (fun candidates mask ->
    List.concat (List.map (fun bits ->
      let pos = Int64.logand mask bits in
      let neg = Int64.logand (Int64.lognot mask) bits in
      (if pos = 0L then [] else [pos]) @ (if neg = 0L then [] else [neg])) candidates)) [bits]
    [ 0x00000000ffffffffL;
      0x0000ffff0000ffffL;
      0x00ff00ff00ff00ffL;
      0x0f0f0f0f0f0f0f0fL;
      0x3333333333333333L;
      0x5555555555555555L ]
  |> List.map Move.of_bits

let count_legal_moves : t -> int = fun b ->
  population_count (legal_moves_aux b)

let perform_move_aux : t -> Move.t -> int64 = fun b m ->
  List.map (fun (mask, shifter) ->
    let masked_enemy = Int64.logand b.enemy_board mask in
    let rec loop t =
      let t' = Int64.logor t (Int64.logand masked_enemy (shifter t)) in
      if t = t' then t
      else loop t' in
    let rev = loop (Int64.logand masked_enemy (shifter (Move.to_bits m))) in
    if Int64.logand b.my_board (shifter rev) = 0L then 0L
    else rev)
    [ (* left *)
      (0x7e7e7e7e7e7e7e7eL, fun t -> Int64.shift_left t 1);
      (* right *)
      (0x7e7e7e7e7e7e7e7eL, fun t -> Int64.shift_right_logical t 1);
      (* upto *)
      (0x00ffffffffffff00L, fun t -> Int64.shift_left t 8);
      (* downto *)
      (0x00ffffffffffff00L, fun t -> Int64.shift_right_logical t 8);
      (* upper right *)
      (0x007e7e7e7e7e7e00L, fun t -> Int64.shift_left t 7);
      (* upper left *)
      (0x007e7e7e7e7e7e00L, fun t -> Int64.shift_left t 9);
      (* lower right *)
      (0x007e7e7e7e7e7e00L, fun t -> Int64.shift_right_logical t 9);
      (* upper right *)
      (0x007e7e7e7e7e7e00L, fun t -> Int64.shift_right_logical t 7) ]
  |> List.fold_left Int64.logor 0L

let perform_move : Move.t -> t -> t = fun  m b ->
  let pos = Move.to_bits m in
  let rev = perform_move_aux b m in
  if rev = 0L
      || Int64.logand pos b.my_board <> 0L
      || Int64.logand pos b.enemy_board <> 0L then
    raise (Invalid_argument "Board.perform_move")
  else
    { my_board = Int64.logor b.my_board (Int64.logor pos rev);
      enemy_board = Int64.logxor b.enemy_board rev }
