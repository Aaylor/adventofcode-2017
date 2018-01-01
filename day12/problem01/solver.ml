
open Aoc_lib


(* Evaluation *)

let find_or_empty key map =
  match IntMap.find_opt key map with
  | None -> IntSet.empty
  | Some set -> set

let eval_input map =
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
  let result = aux_eval 0 IntSet.empty map in
  IntSet.cardinal result


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
