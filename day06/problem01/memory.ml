
module StringSet = Set.Make(String)

let layout_length = 16

(* Returns the index of the the maximum value in the layout. *)
let find_max layout =
  let rec aux_find_max ~index ~index_max ~value_max =
    if index >= layout_length then
      index_max
    else
      let value = layout.(index) in
      let higher = value > value_max in
      aux_find_max
        ~index:(succ index)
        ~index_max:(if higher then index else index_max)
        ~value_max:(if higher then layout.(index) else value_max)
  in
  aux_find_max ~index:0 ~index_max:0 ~value_max:0

(* Delete the value of the maximum value in the layout, and dispatch the remain
   value in other bank. *)
let remove_and_share_blocks layout index =
  (* Delete the value *)
  let shared_block = layout.(index) in
  layout.(index) <- 0;
  (* Dispatch values *)
  let next_index index =
    let index' = succ index in
    if index' >= layout_length then 0 else index'
  in
  let rec share_block current_index shared_block =
    if shared_block <> 0 then begin
      layout.(current_index) <- succ layout.(current_index);
      share_block (next_index current_index) (pred shared_block)
    end
  in
  share_block (next_index index) shared_block

(* The serialization is made as a string sequence of each number. *)
let serialize_layout layout =
  let buffer = Buffer.create 13 in
  Array.iter
    (fun element ->
       Buffer.add_string buffer (string_of_int element);
       Buffer.add_char buffer ';')
    layout;
  Buffer.contents buffer

let steps layout =
  let rec steps_aux steps set =
    (* Serialize and store current layout *)
    let serialized_layout = serialize_layout layout in
    (* If it exists stop now *)
    if StringSet.mem serialized_layout set
    then steps
    else begin
      let set' = StringSet.add serialized_layout set in
      let index = find_max layout in
      remove_and_share_blocks layout index;
      steps_aux (succ steps) set'
    end
  in
  steps_aux 0 StringSet.empty


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

let input_memory_layout channel =
  try
    let line = input_line channel in
    (* We assume that the input is always separated by tabulations *)
    let layout_str = String.split_on_char '\t' line in
    Array.of_list (List.map int_of_string layout_str)
  with
  | End_of_file ->
    failwith "Expected at least one line"

let () =
  let layout = with_channel "input" input_memory_layout in
  let result = steps layout in
  Format.printf "%d@." result
