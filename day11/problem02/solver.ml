
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

let incr_move move map =
  let old_count = MoveMap.find move map in
  MoveMap.add move (succ old_count) map

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

let eval_steps map =
  let rec fixpoint map =
    let map' =
      simplify_contraries_moves map |>
      simplify_diagonal_moves
    in
    if MoveMap.compare compare map map' = 0
    then map'
    else fixpoint map'
  in
  let map' = fixpoint map in
  MoveMap.fold (fun _ v acc -> v + acc) map' 0

let eval_moves moves =
  let rec aux_eval steps map moves =
    match moves with
    | [] ->
      steps
    | move :: moves ->
      let map' = incr_move move map in
      let steps' = eval_steps map' in
      aux_eval (max steps steps') map' moves
  in
  aux_eval 0 empty_map moves


(* Input *)

let with_channel filename fn =
  let channel = open_in filename in
  try
    let result = fn channel in
    close_in channel;
    result
  with exn ->
    close_in channel;
    raise exn

let extract_moves channel =
  try
    let line = input_line channel in
    let line' = String.split_on_char ',' line in
    List.map move_of_string line'
  with End_of_file ->
    failwith "Input need at least one line."


let () =
  let moves = with_channel "input" extract_moves in
  let result = eval_moves moves in
  Format.printf "%d@." result
