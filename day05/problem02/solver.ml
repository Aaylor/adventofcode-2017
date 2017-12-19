
let process_instructions instructions =
  let lower_bound, higher_bound = 0, Array.length instructions - 1 in
  let rec do_process steps idx =
    if idx < lower_bound || idx > higher_bound
    then steps
    else
      let value = instructions.(idx) in
      let next_idx = idx + value in
      instructions.(idx) <- if value >= 3 then value - 1 else value + 1;
      do_process (succ steps) next_idx
  in
  do_process 0 0


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

let instructions channel =
  let rec read_instruction acc =
    try
      let line = input_line channel in
      let instruction = int_of_string line in
      read_instruction (instruction :: acc)
    with
    | End_of_file ->
      Array.of_list (List.rev acc)
    | Failure _ (* int_of_string *) ->
      failwith "Invalid input: integer expected."
  in
  read_instruction []

let () =
  let instructions = with_channel "input" instructions in
  let result = process_instructions instructions in
  Format.printf "%d@." result
