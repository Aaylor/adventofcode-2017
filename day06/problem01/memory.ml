
module ISet = Set.Make(Int32)

let int32_pow a b =
  let rec aux res b =
    if b = 0 then res
    else aux (Int32.mul res a) (pred b)
  in
  aux Int32.one b

let layout_length = 16

let find_max layout =
  let rec aux_find_max idx max_idx max_value =
    if idx >= layout_length then
      max_idx
    else
      let higher = layout.(idx) > max_value in
      aux_find_max
        (succ idx)
        (if higher then idx else max_idx)
        (if higher then layout.(idx) else max_value)
  in
  aux_find_max 0 0 0

let remove_and_share_blocks layout idx =
  let shared_block = layout.(idx) in
  layout.(idx) <- 0;
  let next_idx idx =
    let idx' = succ idx in
    if idx' >= layout_length then 0 else idx'
  in
  let rec share_block current_idx shared_block =
    if shared_block <> 0 then begin
      layout.(current_idx) <- layout.(current_idx) + 1;
      share_block (next_idx current_idx) (pred shared_block)
    end
  in
  share_block (next_idx idx) shared_block

let serialize_layout layout =
  let _, result =
    Array.fold_left
      (fun (idx, value) element ->
         succ idx,
         Int32.add value (int32_pow (Int32.of_int element) idx))
      (0, Int32.zero) layout
  in
  result

let steps layout =
  let rec steps_aux steps map =
    (* Serialize and store current layout *)
    let serialized_layout = serialize_layout layout in
    (* If it exists stop now *)
    if ISet.mem serialized_layout map
    then steps
    else begin
      let map' = ISet.add serialized_layout map in
      let idx = find_max layout in
      remove_and_share_blocks layout idx;
      steps_aux (succ steps) map'
    end
  in
  steps_aux 0 ISet.empty


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
