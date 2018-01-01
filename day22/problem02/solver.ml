
type coordinate =
  { x: int;
    y: int; }

type move =
  [ `SameDirection
  | `Turn of [ `Left | `Right ]
  | `Reverse ]

type direction =
  | South
  | West
  | East
  | North

module CoordinateMap = Map.Make(struct
    type t = coordinate
    let compare = compare
  end)

type cell_state =
  | Infected
  | Clean
  | Weakened
  | Flagged

let get coordinate map =
  match CoordinateMap.find_opt coordinate map with
  | None -> Clean
  | Some state -> state

let set state coordinate map =
  CoordinateMap.add coordinate state map

let infect = set Infected
let clean = set Clean
let weakened = set Weakened
let flagged = set Flagged

let next_coordinate coordinate move =
  match move with
  | South -> { coordinate with y = coordinate.y + 1 }
  | North -> { coordinate with y = coordinate.y - 1 }
  | West -> { coordinate with x = coordinate.x - 1 }
  | East -> { coordinate with x = coordinate.x + 1 }

let change_direction current_direction move =
  match move with
  | `SameDirection ->
    current_direction
  | `Reverse ->
    begin
      match current_direction with
      | South -> North
      | North -> South
      | East -> West
      | West -> East
    end
  | `Turn move ->
    match current_direction with
    | South -> if move = `Left then East else West
    | North -> if move = `Left then West else East
    | East -> if move = `Left then North else South
    | West -> if move = `Left then South else North

let sporifica_virus max_bursts (coordinate, map) =
  let rec do_moves index infections coordinate direction map =
    if index < max_bursts then
      let infections, next_move, map' =
        match get coordinate map with
        | Clean -> infections, `Turn `Left, weakened coordinate map
        | Weakened -> infections + 1, `SameDirection, infect coordinate map
        | Infected -> infections, `Turn `Right, flagged coordinate map
        | Flagged -> infections, `Reverse, clean coordinate map
      in
      let next_direction = change_direction direction next_move in
      let coordinate' = next_coordinate coordinate next_direction in
      do_moves (succ index) infections coordinate' next_direction map'
    else
      infections
  in
  do_moves 0 0 coordinate North map


(* INPUT *)

let with_channel filename fn =
  let channel = open_in filename in
  try
    let result = fn channel in
    close_in channel;
    result
  with exn ->
    close_in channel;
    raise exn

let foldi_string fn acc line =
  let length = String.length line in
  let rec aux_fold index acc =
    if index >= length then
      acc
    else
      let c = line.[index] in
      let acc' = fn index acc c in
      aux_fold (succ index) acc'
  in
  aux_fold 0 acc

let extract_lines channel =
  let rec exhaust acc =
    try exhaust (input_line channel :: acc)
    with End_of_file -> List.rev acc
  in
  (* We consider here that the input IS ALWAYS a square. *)
  let matrix_list = exhaust [] in
  let matrix_length = List.length matrix_list in
  (* Evaluate the start point *)
  let coordinate_start = { x = matrix_length / 2; y = matrix_length / 2; } in
  (* And then, store to the map every infected cells.  *)
  let _, map =
    List.fold_left
      (fun (index_y, map) line ->
         let map' =
           foldi_string
             (fun index_x map character ->
                if character = '#' then
                  let coordinate = { x = index_x; y = index_y } in
                  CoordinateMap.add coordinate Infected map
                else
                  map)
             map
             line
         in
         succ index_y, map')
      (0, CoordinateMap.empty)
      matrix_list
  in
  coordinate_start, map

let bursts = 10_000_000

let () =
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_custom extract_lines)
    ~aoc_solver:(sporifica_virus bursts)
    ~aoc_printer:string_of_int
