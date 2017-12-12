
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

let do_round ~start_position ~skip_size lengths =
  let rec round ~start_position ~skip_size ~lengths =
    match lengths with
    | [] ->
      start_position, skip_size
    | length :: lengths ->
      eval_length ~start_position length;
      let start_position = add_index start_position (skip_size + length) in
      round ~start_position ~skip_size:(succ skip_size) ~lengths
  in
  round ~start_position ~skip_size ~lengths

let do_rounds lengths =
  let rec aux_rounds ~start_position ~skip_size count =
    if count > 0 then begin
      let start_position, skip_size =
        do_round ~start_position ~skip_size lengths
      in
      aux_rounds ~start_position ~skip_size (pred count)
    end
  in
  aux_rounds ~start_position:0 ~skip_size:0 64

let hash_part block =
  (* This evaluates the hash value of a block of 16 elements
     from the array. *)
  let index_start = block * 16 in
  let index_stop = (block + 1) * 16 in
  let rec eval_hash index acc =
    if index < index_stop
    then eval_hash (succ index) (acc lxor (get index))
    else acc
  in
  eval_hash index_start 0

let do_hash lengths =
  (* Do all the rounds *)
  do_rounds lengths;
  (* Evaluate each blocks and print it to the buffer *)
  let buffer = Buffer.create 13 in
  let blocks = array_size / 16 in
  let rec evaluate_blocks current_block =
    if current_block < blocks then begin
      (* Evaluate the hash of the block, and add the hexadecimal value
         of the result to the buffer. *)
      let hash = hash_part current_block in
      let hash_hex = Format.sprintf "%02x" hash in
      Buffer.add_string buffer hash_hex;
      evaluate_blocks (succ current_block)
    end
  in
  evaluate_blocks 0;
  (* When all blocks have been evaluated, we can just returns the string
     value of the buffer. *)
  Buffer.contents buffer


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

let extract_ascii_characters ~length line =
  let rec aux_eval acc index =
    if index < 0 then
      acc
    else
      let c = Char.code line.[index] in
      aux_eval (c :: acc) (pred index)
  in
  aux_eval [] (length -1)

let input_lengths channel =
  try
    let line = input_line channel in
    let length = String.length line in
    let input = extract_ascii_characters ~length line in
    input @ [17; 31; 73; 47; 23]
  with End_of_file ->
    failwith "The input must have at least one line"

let () =
  let lengths = with_channel "input" input_lengths in
  let result = do_hash lengths in
  Format.printf "%s@." result
