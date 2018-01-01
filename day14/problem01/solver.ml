
module type SOLVER_INPUT = sig
  val lengths: int list
end

module type SOLVER = sig
  val solve: unit -> int
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

  let solve () =
    (* Do all the rounds *)
    do_rounds S.lengths;
    (* Evaluate each blocks and print it to the buffer *)
    let blocks = array_size / 16 in
    let rec evaluate_blocks bits current_block =
      if current_block < blocks then
        let hash = hash_part current_block in
        let bits' = count_set_bits hash in
        evaluate_blocks (bits + bits') (succ current_block)
      else
        bits
    in
    evaluate_blocks 0 0
end


(* INPUT *)

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

let solve l =
  let (module Solver: SOLVER) = (module Solver(struct let lengths = l end)) in
  Solver.solve ()

let iterate_inputs input =
  let rec aux_iterate bits index =
    if index < input_max then
      (* Calculate the input. *)
      let input = Format.sprintf "%s-%d" input index in
      let lengths = get_lengths input in
      (* Get the number of used bits and loop to next index *)
      let bits' = solve lengths in
      aux_iterate (bits + bits') (succ index)
    else
      bits
  in
  aux_iterate 0 0

let () =
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_static "xlqgujun")
    ~aoc_solver:iterate_inputs
    ~aoc_printer:string_of_int
