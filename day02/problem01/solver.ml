
let do_check op element input =
  match element with
  | None -> Some input
  | Some element -> Some (if op element input then element else input)

let check_min min input = do_check ( < ) min input
let check_max max input = do_check ( > ) max input

let eval matrix =
  let eval_row (min, max) element =
    (** For each element update the minimum and the maximum element of
        the list. *)
    let min' = check_min min element in
    let max' = check_max max element in
    min', max'
  in
  let eval_rows sum row =
    let row_result = List.fold_left eval_row (None, None) row in
    match row_result with
    | Some min, Some max -> sum + (max - min)
    | _ -> failwith "bad input"
  in
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
