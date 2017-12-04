
(* To solve the problem, we just construct the spiral, and stops at the
   first value larger than the input. *)

(** {2 Basic Coordinate} *)

type coordinate =
  { x: int;
    y: int; }

(* Basic move on a matrix. *)
let up coordinate = { coordinate with y = coordinate.y - 1 }
let bottom coordinate = { coordinate with y = coordinate.y + 1 }
let right coordinate = { coordinate with x = coordinate.x + 1 }
let left coordinate = { coordinate with x = coordinate.x - 1 }

(* Value extraction. *)

let get array coordinate =
  if coordinate.y < 0 || coordinate.y >= 23 ||
     coordinate.x < 0 || coordinate.x >= 23
  then
    None
  else
    array.(coordinate.y).(coordinate.x)

let set array coordinate value =
  array.(coordinate.y).(coordinate.x) <- Some value


(** {2 Helper} *)

(* Repeat the function [fn] [n] times, and returns the last value
   used. *)
let repeat n fn value =
  let rec aux count value =
    if count >= n then value
    else aux (succ count) (fn value)
  in
  aux 0 value


(** {2 The solution} *)

let input = 361527

let array_size =
  (* For this, we just take a static size that just fit for the
     given input. *)
  13

let array =
  (* Create the array and init the centered cell to 1. *)
  let array = Array.make_matrix array_size array_size None in
  let center = array_size / 2 in
  array.(center).(center) <- Some 1;
  array

let pretty_array () =
  Array.iter
    (fun array ->
       Array.iter
         (fun value ->
            match value with
            | None -> Format.printf "%8s " "None"
            | Some v -> Format.printf "%8d " v)
         array;
       Format.printf "@.")
    array

let eval_cell coordinate =
  let cells n = [ n - 1; n; n + 1 ] in
  List.fold_left
    (fun sum y ->
       List.fold_left
         (fun sum x ->
            match get array { x; y } with
            | None -> sum
            | Some value -> value + sum)
         sum (cells coordinate.x))
    0 (cells coordinate.y)

let eval_squares coordinates =
  let exception Result_found of int in
  let wrap_eval_cell move coordinate =
    (* This is simple wrapper for eval_cell:
         - it moves the selected cell
         - it evaluates the cell value
         - if the wanted value have been found, it raises the exception
           otherwise it set the value on the array and returns the coordinate *)
    let next = move coordinate in
    let value = eval_cell next in
    if value > input then raise (Result_found value);
    set array next value;
    next
  in
  let rec aux_eval_squares axis coordinate =
    let steps_to_do = 2 * axis in

    (* We need to do a move to the right first. *)
    wrap_eval_cell right coordinate |>

    (* Then, do (2 * axis - 1) to the up *)
    repeat (steps_to_do - 1) (wrap_eval_cell up) |>

    (* And then repeat the same steps for each side *)
    repeat steps_to_do (wrap_eval_cell left) |>
    repeat steps_to_do (wrap_eval_cell bottom) |>
    repeat steps_to_do (wrap_eval_cell right) |>

    (* And then, after filling the square, we just call recursively
       the same function in next axis. *)
    aux_eval_squares (succ axis)
  in
  try aux_eval_squares 1 coordinates
  with Result_found i -> i

let () =
  let init_coordinate = { x = array_size / 2; y = array_size / 2 } in
  let result = eval_squares init_coordinate in
  (* pretty_array (); *)
  Format.printf "%d@." result
