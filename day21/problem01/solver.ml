
module StringMap = Map.Make(String)

(* Fractal *)

(* This function does the transformation for every group starting at the row
   [start_row].

   If the current input is a 4x4 matrix, and the start_row is 0, it will
   transform the two groups (starting at (0,0) and (2,0); ending respectively
   at (1, 1) and (3, 3)). *)
let transform_row_groups map start_row group_size input =
  (* The transformation is always (group_size + 1). So allocate as
     many buffer as we need to fill buffers.*)
  let array_buffers = Array.init (group_size + 1) (fun _ -> Buffer.create 3) in

  (* Determine the limit before splitting groups. *)
  let max_row = start_row + group_size in
  let max_column = Array.length input in

  (* This sub-function extract all groups and do the transformation for each
     group. *)
  let rec transform_group start_x =
    let rec extract_group current_row acc =
      if current_row >= max_row then
        acc
      else
        let line = input.(current_row) in
        let sub_part = String.sub line start_x group_size in
        let acc =
          if acc = "" then sub_part
          else Format.sprintf "%s/%s" acc sub_part
        in
        extract_group (succ current_row) acc
    in
    if start_x < max_column then begin
      (* Extract and transform the group *)
      let group = extract_group start_row "" in
      let transformation =
        match StringMap.find_opt group map with
        | None -> assert false
        | Some transformation -> transformation
      in
      (* Split the transformation and append each cells of the list to the
         correct buffer. *)
      List.iteri
        (fun i s -> Buffer.add_string array_buffers.(i) s)
        (String.split_on_char '/' transformation);
      (* Continue to loop on groups. *)
      transform_group (start_x + group_size)
    end
  in
  transform_group 0;

  (* Construct the final transformation for transformed rows *)
  let buffer = Buffer.create 13 in
  Array.iteri
    (fun i b ->
       if i <> 0 then Buffer.add_char buffer '/';
       Buffer.add_buffer buffer b)
    array_buffers;
  Buffer.contents buffer


let transform_rows map input =
  (* Basic information on the input:
       - Rows (transformed into array: easier to use)
       - The size of group to work with *)
  let rows = Array.of_list (String.split_on_char '/' input) in
  let rows_length = Array.length rows in
  let group_size = if Array.length rows mod 2 = 0 then 2 else 3 in

  (* This part iterates on "group" of rows, doing the transformation and
     appending the result on the buffer. *)
  let buffer = Buffer.create 13 in
  let rec loop_on_rows start_row =
    if start_row < rows_length then begin
      (* Transform the rows. *)
      let result = transform_row_groups map start_row group_size rows in
      (* Append the result to the buffer *)
      if start_row <> 0 then Buffer.add_char buffer '/';
      Buffer.add_string buffer result;
      loop_on_rows (start_row + group_size)
    end
  in
  loop_on_rows 0;
  Buffer.contents buffer

let transform_input map input iteration =
  let rec transform_loop index input =
    if index < iteration then
      let input' = transform_rows map input in
      transform_loop (succ index) input'
    else
      input
  in
  transform_loop 0 input

let count_pixel_on pattern =
  let length = String.length pattern in
  let rec aux_count index count =
    if index >= length then
      count
    else
      let count = if pattern.[index] = '#' then succ count else count in
      aux_count (succ index) count
  in
  aux_count 0 0


(* Transformation storage *)

let matrix_to_string matrix =
  let buffer = Buffer.create 4 in
  Array.iteri
    (fun i a ->
       if i <> 0 then Buffer.add_char buffer '/';
       Array.iter (Buffer.add_char buffer) a)
    matrix;
  Buffer.contents buffer

let with_input line map =
  let rows = String.split_on_char '/' line in
  let matrix_length = List.length rows in
  let matrix = Array.make_matrix matrix_length matrix_length '.' in
  let map' = map matrix matrix_length in
  List.iteri (fun j s -> String.iteri (fun i c -> map' (i, j) c) s) rows;
  matrix_to_string matrix

let flip matrix matrix_length (i, j) c =
  matrix.(i).(abs (j - matrix_length + 1)) <- c

let symmetry matrix _length (i, j) c =
  matrix.(i).(j) <- c

let store_transformations map initial_input transformation =
  let rec aux_iterate idx map current_input =
    if idx <= 8 then
      let key1 = with_input current_input symmetry in
      let map' = StringMap.add key1 transformation map in
      let key2 = with_input current_input flip in
      let map' = StringMap.add key2 transformation map' in
      aux_iterate (idx + 2) map' key2
    else
      map
  in
  aux_iterate 0 map initial_input


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

let fold_lines channel =
  let rec aux_fold_lines map =
    try
      let line = input_line channel in
      let input, transformation =
        Scanf.sscanf line
          "%s => %s"
          (fun s1 s2 -> s1, s2)
      in
      let map' = store_transformations map input transformation in
      aux_fold_lines map'
    with End_of_file ->
      map
  in
  aux_fold_lines StringMap.empty

let start_pattern = ".#./..#/###"
let iteration = 5

let () =
  let map = with_channel "input" fold_lines in
  let final_pattern = transform_input map start_pattern iteration in
  let count = count_pixel_on final_pattern in
  Format.printf "%d@." count
