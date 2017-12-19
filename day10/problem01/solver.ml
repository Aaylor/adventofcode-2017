
(* Array & Helpers *)

let array_size = 256
let knot_array = Array.init array_size (fun i -> i)

let set i v = knot_array.(i) <- v
let get i = knot_array.(i)

let swap i j =
  let v = get i in
  set i (get j);
  set j v

let add_index index increment =
  (index + increment) mod array_size

let sub_index index decrement =
  let v = index - decrement in
  if v < 0 then array_size + v
  else v


(* The solution *)

let eval_length ~start_position length =
  let min_index = start_position in
  let max_index = add_index start_position (length - 1) in
  let rec loop min_index max_index loop_turn =
    if loop_turn <> 0 then begin
      swap min_index max_index;
      loop (add_index min_index 1) (sub_index max_index 1) (pred loop_turn)
    end
  in
  loop min_index max_index (length / 2)

let do_knot_hash lengths =
  let rec aux_knot_hash ~start_position ~skip_size ~lengths =
    match lengths with
    | [] ->
      get 0 * get 1
    | length :: lengths ->
      eval_length ~start_position length;
      let start_position = add_index start_position (skip_size + length) in
      aux_knot_hash ~start_position ~skip_size:(succ skip_size) ~lengths
  in
  aux_knot_hash ~start_position:0 ~skip_size:0 ~lengths


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

let input_lengths channel =
  try
    let line = input_line channel in
    let elements = String.split_on_char ',' line in
    List.map int_of_string elements
  with End_of_file ->
    failwith "The input must have at least one line"

let () =
  let lengths = with_channel "input" input_lengths in
  let result = do_knot_hash lengths in
  Format.printf "%d@." result
