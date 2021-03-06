
(* Movements *)

type move =
  | N | NE | NW
  | S | SE | SW

let string_of_move = function
  | N -> "n"
  | NE -> "ne"
  | NW -> "nw"
  | S -> "s"
  | SE -> "se"
  | SW -> "sw"

let move_of_string = function
  | "n" -> N
  | "ne" -> NE
  | "nw" -> NW
  | "s" -> S
  | "se" -> SE
  | "sw" -> SW
  | s -> failwith ("invalid move " ^ s)

module MoveMap = Map.Make(struct
    type t = move
    let compare = compare
  end)

let empty_map =
  List.fold_left
    (fun acc k -> MoveMap.add k 0 acc)
    MoveMap.empty
    [ N; S; NE; NW; SE; SW ]


(* Evaluate moves *)

let count_moves moves =
  List.fold_left
    (fun map move ->
       let old_count = MoveMap.find move map in
       MoveMap.add move (succ old_count) map)
    empty_map
    moves

let simplify_contraries_moves map =
  List.fold_left
    (fun map (m1, m2) ->
       let count_m1', count_m2' =
         let count_m1 = MoveMap.find m1 map in
         let count_m2 = MoveMap.find m2 map in
         if count_m1 = count_m2 then 0, 0
         else if count_m1 < count_m2 then 0, (count_m2 - count_m1)
         else (count_m1 - count_m2), 0
       in
       let map' = MoveMap.add m1 count_m1' map in
       MoveMap.add m2 count_m2' map')
    map
    [ (N, S); (NE, SW); (NW, SE) ]

let simplify_diagonal_moves map =
  List.fold_left
    (fun map (m1, m2, result) ->
       let count_m1', count_m2', count_result =
         let count_m1 = MoveMap.find m1 map in
         let count_m2 = MoveMap.find m2 map in
         let count_result = MoveMap.find result map in
         let min_count = min count_m1 count_m2 in
         count_m1 - min_count, count_m2 - min_count, count_result + min_count
       in
       MoveMap.add m1 count_m1' map |>
       MoveMap.add m2 count_m2' |>
       MoveMap.add result count_result)
    map
    [ (SE, SW, S); (NE, NW, N); (NE, S, SE); (NW, S, SW); (SW, N, NW);
      (SE, N, NE) ]

let eval_moves moves =
  let rec fixpoint map =
    let map' =
      simplify_contraries_moves map |>
      simplify_diagonal_moves
    in
    if MoveMap.compare compare map map' = 0
    then map'
    else fixpoint map'
  in
  let count_map = count_moves moves in
  let result_map = fixpoint count_map in
  MoveMap.fold (fun _ v acc -> v + acc) result_map 0


(* INPUT *)

let () =
  let parse_line l = List.map move_of_string (String.split_on_char ',' l) in
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_single_line parse_line)
    ~aoc_solver:eval_moves
    ~aoc_printer:string_of_int
