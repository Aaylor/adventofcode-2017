
open Aoc_lib

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
  let update_layout () = remove_and_share_blocks layout (find_max layout) in
  let rec first_repeated_layout map =
    (* Serialize and store current layout *)
    let serialized_layout = serialize_layout layout in
    (* If it exists stop now *)
    let map' = StringSet.add serialized_layout map in
    if StringSet.mem serialized_layout map
    then serialized_layout
    else begin
      update_layout ();
      first_repeated_layout map'
    end
  in
  let rec find_second_repeat expected_layout steps =
    let serialized_layout = serialize_layout layout in
    if String.equal expected_layout serialized_layout
    then steps
    else begin
      update_layout ();
      find_second_repeat expected_layout (succ steps)
    end
  in
  let repeated_layout = first_repeated_layout StringSet.empty in
  (* Do not forget to run one turn before running the second find; and do not
     forget to add one (since we move one step forward). *)
  update_layout ();
  find_second_repeat repeated_layout 1


(* INPUT *)

let () =
  let map_line line =
    let layout_string = String.split_on_char '\t' line in
    Array.of_list (List.map int_of_string layout_string)
  in
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_single_line map_line)
    ~aoc_solver:steps
    ~aoc_printer:string_of_int
