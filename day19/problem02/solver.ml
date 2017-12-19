
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
  let rec do_move ~steps coordinate move =
    let steps = succ steps in
    match get graph coordinate with
    | None ->
      failwith "The coordinate should not be outside the board."
    | Some ' ' ->
      (* Reaching the blank cell made the little packet doing one more
         move then needed. *)
      steps - 1
    | Some '+' ->
      let move' = next_move graph coordinate move in
      let coordinate' = next_coordinate coordinate move' in
      do_move ~steps coordinate' move'
    | Some ('|' | '-' | 'A'..'Z') ->
      do_move ~steps (next_coordinate coordinate move) move
    | _ ->
      failwith "Unknown character."
  in
  do_move ~steps:0 (entry_point graph) Down


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

let construct_graph channel =
  let rec extract_lines acc =
    try
      let line = input_line channel in
      extract_lines (line :: acc)
    with End_of_file ->
      Array.of_list (List.rev acc)
  in
  extract_lines []

let () =
  let graph = with_channel "input" construct_graph in
  let result = find_way graph in
  Format.printf "%d@." result
