
(** {2 Coordinates} *)

type coordinate =
  { x: int;
    y: int }

module CoordinateSet = Set.Make(struct
    type t = coordinate
    let compare = compare
  end)

module CoordinateTbl = Hashtbl.Make(struct
    type t = coordinate
    let hash = Hashtbl.hash
    let equal = ( = )
  end)


(** {2 Identifiers} *)

module IdentifierTbl = Hashtbl.Make(struct
    type t = int
    let hash = Hashtbl.hash
    let equal = ( = )
  end)


(** {2 Graph} *)

module Graph: sig
  val store_node: coordinate -> unit
  val groups: unit -> int
end = struct
  let coordinate_table = CoordinateTbl.create 13
  let identifier_table = IdentifierTbl.create 13

  let generate_identifier =
    let identifier = ref 0 in
    fun () ->
      let result = !identifier in
      incr identifier;
      result

  let store_identifier_coordinate identifier coordinate =
    let singleton = CoordinateSet.singleton coordinate in
    let coordinate_set =
      match IdentifierTbl.find_opt identifier_table identifier with
      | None -> singleton
      | Some set -> CoordinateSet.union set singleton
    in
    IdentifierTbl.replace identifier_table identifier coordinate_set

  let store_coordinate identifier coordinate =
    store_identifier_coordinate identifier coordinate;
    CoordinateTbl.replace coordinate_table coordinate identifier

  let merge_identifier identifier_from identifier_to =
    match IdentifierTbl.find_opt identifier_table identifier_from with
    | None -> ()
    | Some coordinate_set ->
      (* We modify the identifier of each coordinate. *)
      CoordinateSet.iter
        (fun coordinate -> store_coordinate identifier_to coordinate)
        coordinate_set;
      (* Then, clean the identifier *)
      IdentifierTbl.remove identifier_table identifier_from;
      (* Merge the coordinate tables *)
      let set' =
        match IdentifierTbl.find_opt identifier_table identifier_to with
        | None -> CoordinateSet.empty
        | Some set' -> set'
      in
      let uset = CoordinateSet.union coordinate_set set' in
      IdentifierTbl.replace identifier_table identifier_to uset

  let left_cell_identifier coordinate =
    let coordinate' = { coordinate with x = coordinate.x - 1 } in
    CoordinateTbl.find_opt coordinate_table coordinate'

  let top_cell_identifier coordinate =
    let coordinate' = { coordinate with y = coordinate.y - 1 } in
    CoordinateTbl.find_opt coordinate_table coordinate'

  let store_node coordinate =
    let identifier =
      match left_cell_identifier coordinate, top_cell_identifier coordinate with
      | None, None ->
        (* The left and top cell are not marked. Generate a new identifier. *)
        generate_identifier ()
      | None, Some identifier | Some identifier, None ->
        (* The left or the top cell are marked: take the identifier of this cell
           since it forms a group. *)
        identifier
      | Some left_identifier, Some top_identifier ->
        (* There is a conflict between two groups. Merge them. *)
        merge_identifier left_identifier top_identifier;
        top_identifier
    in
    store_coordinate identifier coordinate

  let groups () =
    IdentifierTbl.length identifier_table
end

module type SOLVER_INPUT = sig
  val lengths: int list
end

module type SOLVER = sig
  val solve: int -> unit
end

(* Nasty way to do this but... Since we rely on an imperative array,
   let's get some fun with some functors, yay! *)
module Solver(S: SOLVER_INPUT): SOLVER = struct
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

  let count_set_bits n =
    let rec loop count n =
      if n = 0 then count
      else loop (succ count) (n land (n - 1))
    in
    loop 0 n

  let char_to_binary = function
    | '0' -> "0000" | '1' -> "0001" | '2' -> "0010" | '3' -> "0011"
    | '4' -> "0100" | '5' -> "0101" | '6' -> "0110" | '7' -> "0111"
    | '8' -> "1000" | '9' -> "1001" | 'a' -> "1010" | 'b' -> "1011"
    | 'c' -> "1100" | 'd' -> "1101" | 'e' -> "1110" | 'f' -> "1111"
    | c -> Format.printf "Invalid '%c'@." c; assert false

  let binary_representation hash =
    let hex = Format.sprintf "%02x" hash in
    Format.sprintf "%s%s" (char_to_binary hex.[0]) (char_to_binary hex.[1])

  let fill_array line_no block_no hash =
    let binary = binary_representation hash in
    String.iteri
      (fun i c ->
         if c = '1' then
           let x = (block_no * 8) + i in
           Graph.store_node { x; y = line_no })
      binary

  let solve line_no =
    (* Do all the rounds *)
    do_rounds S.lengths;
    (* Evaluate each blocks and print it to the buffer *)
    let blocks = array_size / 16 in
    let rec evaluate_blocks current_block =
      if current_block < blocks then begin
        let hash = hash_part current_block in
        fill_array line_no current_block hash;
        evaluate_blocks (succ current_block)
      end
    in
    evaluate_blocks 0
end


(* INPUT *)

let input = "xlqgujun"
let input_max = 128

let extract_ascii_characters ~length line =
  let rec aux_eval acc index =
    if index < 0 then
      acc
    else
      let c = Char.code line.[index] in
      aux_eval (c :: acc) (pred index)
  in
  aux_eval [] (length - 1)

let get_lengths input =
  let length = String.length input in
  let input = extract_ascii_characters ~length input in
  input @ [17; 31; 73; 47; 23]

let solve line_no l =
  let (module Solver: SOLVER) = (module Solver(struct let lengths = l end)) in
  Solver.solve line_no

let iterate_inputs () =
  let rec aux_iterate index =
    if index < input_max then begin
      (* Calculate the input. *)
      let input = Format.sprintf "%s-%d" input index in
      let lengths = get_lengths input in
      (* Get the number of used bits and loop to next index *)
      solve index lengths;
      aux_iterate (succ index)
    end
  in
  aux_iterate 0

let () =
  iterate_inputs ();
  Format.printf "%d@." (Graph.groups ());
