
type coordinate =
  { x: int;
    y: int; }

type move =
  | Down
  | Left
  | Right
  | Up

let get graph coordinate =
  let y_length = Array.length graph in
  if coordinate.y >= y_length || coordinate.y < 0 then None
  else
    let x_length = String.length graph.(coordinate.y) in
    if coordinate.x >= x_length || coordinate.x < 0 then None
    else Some graph.(coordinate.y).[coordinate.x]

let next_coordinate coordinate move =
  match move with
  | Down -> { coordinate with y = coordinate.y + 1 }
  | Up -> { coordinate with y = coordinate.y - 1 }
  | Left -> { coordinate with x = coordinate.x - 1 }
  | Right -> { coordinate with x = coordinate.x + 1 }

let next_move graph coordinate move =
  let is_ok c =
    match c with
    | None | Some ' ' -> false
    | _ -> true
  in
  match move with
  | Up | Down ->
    let left = next_coordinate coordinate Left in
    if is_ok (get graph left) then Left else Right
  | Left | Right ->
    let up = next_coordinate coordinate Up in
    if is_ok (get graph up) then Up else Down

let entry_point graph =
  { x = String.index graph.(0) '|';
    y = 0 }

let find_way graph =
  let buffer = Buffer.create 13 in
  let rec do_move coordinate move =
    match get graph coordinate with
    | None ->
      failwith "The coordinate should not be outside the board."
    | Some ' ' ->
      (* We suppose that reaching a blank means the end of the road. *)
      ()
    | Some '+' ->
      let move' = next_move graph coordinate move in
      let coordinate' = next_coordinate coordinate move' in
      do_move coordinate' move'
    | Some ('A'..'Z' as c) ->
      Buffer.add_char buffer c;
      do_move (next_coordinate coordinate move) move
    | Some ('|' | '-') ->
      do_move (next_coordinate coordinate move) move
    | _ ->
      failwith "Unknown character."
  in
  do_move (entry_point graph) Down;
  Buffer.contents buffer


(* INPUT *)

let () =
  let parse_line line acc = line :: acc in
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_all_lines_ordered ~start:[] parse_line)
    ~aoc_solver:(fun l -> find_way (Array.of_list l))
    ~aoc_printer:(fun s -> s)
