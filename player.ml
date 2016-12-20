external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply";;

let alphabeta eval ( <= ) ( ~- ) successors =
  let max x y = if x <= y then y else x in
  let rec alphabeta ~alpha ~beta node = function
    | 0 -> eval node
    | n ->
        match successors node with
        | [] -> eval node
        | nodes ->
            let rec maximum alpha = function
              | [] -> alpha
              | node :: rest ->
                  let alpha = max alpha (~- (alphabeta ~alpha:(~- beta) ~beta:(~- alpha) node (n - 1))) in
                  if beta <= alpha then beta
                  else maximum alpha rest in
            maximum alpha nodes in
  alphabeta

let alphabeta_reversi eval board n =
  alphabeta
    (fun (route, board) -> (route, eval board))
    (fun (_, v1) (_, v2)-> v1 <= v2)
    (fun (route, v) -> (route, -v))
    (fun (route, board) ->
      begin match Board.legal_moves board with
      | [] -> [(None :: route, Board.flip board)]
      | moves ->
          List.map (fun move ->
            (Some move :: route,
             Board.flip (Board.perform_move move board))) moves
      end)
    ~alpha:([], min_int + 1)
    ~beta:([], max_int)
    ([], board)
    n
  |> fst
  |> List.rev
  |> List.hd

module IntMap = Map.Make (struct
  type t = int
  let compare = compare
end)

let table = 
  [| [|  3000; -12;  0; -1; -1;  0; -12;  3000 |];
     [| -12; -15; -3; -3; -3; -3; -15; -12 |];
     [|   0;  -3;  0; -1; -1;  0;  -3;   0 |];
     [|  -1;  -3; -1; -1; -1; -1;  -3;  -1 |];
     [|  -1;  -3; -1; -1; -1; -1;  -3;  -1 |];
     [|   0;  -3;  0; -1; -1;  0;  -3;   0 |];
     [| -12; -15; -3; -3; -3; -3; -15; -12 |];
     [|  3000; -12;  0; -1; -1;  0; -12;  3000 |] |]
let weight = 
  Array.init 8 (fun i ->
    Array.init 8 (fun j -> (i, j)) |> Array.to_list) |> Array.to_list
  |> List.concat
  |> List.fold_left (fun map (i, j) ->
      if table.(i).(j) = 0 then map else
      IntMap.add table.(i).(j)
        (Int64.logor
          (Int64.shift_left 1L (8 * j + i))
          (try IntMap.find table.(i).(j) map with Not_found -> 0L)) map) IntMap.empty
  |> IntMap.bindings
  |> List.map (fun (x, y) -> (y, x))
  |> List.filter (function (_, 0) -> false | _ -> true)

let last_spurt = 20

let play b =
  if Board.count_disks b + last_spurt / 2 <= 64 then
    alphabeta_reversi (fun b -> Board.eval weight b + Board.count_legal_moves b - Board.count_legal_moves (Board.flip b)) b 7
  else
    alphabeta_reversi
      (Board.eval [(0xFFFFFFFFFFFFFFFFL, 1)]) b last_spurt
