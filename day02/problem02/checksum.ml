
let min_max i1 i2 =
  if i1 > i2 then i2, i1 else i1, i2

(** [check_even n l] takes a number [n] and tries to find the first
    element in [l] that evenly divide [n].
    This element may not exists. *)
let rec find_evenly_divide number list =
  match list with
  | [] -> None
  | x :: xs ->
    let (min, max) = min_max number x in
    if max mod min = 0
    then Some (max / min)
    else find_evenly_divide number xs

(** For each element, we check if it's even with one subsequent element.
    The function stops and returns the result when it finds the first
    two elements that evenly divide. *)
let rec process_row list =
  match list with
  | [] ->
    (* With the given input, this should not happen. *)
    assert false
  | x :: xs ->
    match find_evenly_divide x xs with
    | None -> process_row xs
    | Some result -> result

let eval matrix =
  let eval_rows sum row = sum + process_row row in
  List.fold_left eval_rows 0 matrix



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

let input_matrix filename =
  let process_line line =
    (* It assume that the given input has its element separated by
       \t characters, and not spaces. *)
    List.map
      int_of_string
      (String.split_on_char '\t' line)
  in
  let lines channel =
    let rec exhaust lines =
      try
        let line = input_line channel in
        exhaust (process_line line :: lines)
      with End_of_file ->
        lines
    in
    List.rev (exhaust [])
  in
  with_channel filename lines

let () =
  let matrix = input_matrix "input" in
  let result = eval matrix in
  Format.printf "%d@." result
