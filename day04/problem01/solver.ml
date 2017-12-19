
(* The idea here is to sort all words from the line removing duplicates,
   and just check that the length of the two lists are still equal. *)

let is_valid_line line =
  let line = String.split_on_char ' ' line in
  let line_sorted = List.sort_uniq String.compare line in
  List.compare_lengths line line_sorted = 0



(** {2 Input} *)

let with_channel filename fn =
  let channel = open_in filename in
  try
    let result = fn channel in
    close_in channel;
    result
  with exn ->
    close_in channel;
    raise exn

let eval_input channel =
  let rec aux_eval valid_lines =
    try
      let line = input_line channel in
      let valid_lines' =
        if is_valid_line line
        then succ valid_lines
        else valid_lines
      in
      aux_eval valid_lines'
    with End_of_file -> valid_lines
  in
  aux_eval 0

let () =
  let result = with_channel "input" eval_input in
  Format.printf "%d@." result
