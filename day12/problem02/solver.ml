
open Aoc_lib


(* Evaluation *)

let find_or_empty key map =
  match IntMap.find_opt key map with
  | None -> IntSet.empty
  | Some set -> set

let key_set map =
  IntMap.fold (fun key _ acc -> IntSet.add key acc) map IntSet.empty

let eval_group current map =
  (* Evaluate each element of a group until they have been all reached at
     least once. *)
  let rec aux_eval current seen map =
    if IntSet.mem current seen then
      seen
    else
      let to_eval = find_or_empty current map in
      IntSet.fold
        (fun element acc ->
           aux_eval element acc map)
        to_eval
        (IntSet.add current seen)
  in
  aux_eval current IntSet.empty map

let eval_input map =
  let rec aux count set =
    if IntSet.is_empty set then
      count
    else
      (* Evaluate the complete group, and remove all seen member
         in that group.
         When the set is empty, we have the number of process
         groups. *)
      let current = IntSet.choose set in
      let set' = eval_group current map in
      let set' = IntSet.diff set set' in
      aux (succ count) set'
  in
  aux 0 (key_set map)


(* INPUT *)

let extract_value value =
  int_of_string (List.hd (String.split_on_char ',' value))

let process_line line map =
  match String.split_on_char ' ' line with
  | key :: "<->" :: values ->
    let key = int_of_string key in
    let set' =
      List.fold_left
        (fun acc value ->
           let value = extract_value value in
           IntSet.add value acc)
        (find_or_empty key map)
        values
    in
    IntMap.add key set' map
  | _ ->
    failwith "Invalid line"

let () =
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_all_lines ~start:IntMap.empty process_line)
    ~aoc_solver:eval_input
    ~aoc_printer:string_of_int
