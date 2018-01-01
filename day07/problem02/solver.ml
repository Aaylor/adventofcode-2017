
open Aoc_lib

let find_bottom_program (circus, programs_set) =
  let childs =
    StringMap.fold
      (fun _ (_, set) acc -> StringSet.union set acc)
      circus
      StringSet.empty
  in
  let result = StringSet.diff programs_set childs in
  StringSet.choose result

let evaluate_weight circus bottom =
  (* The usage of Hashtbl here instead of Map was made to avoid easily
     stack overflows... *)
  let weight_table = Hashtbl.create 13 in
  let rec aux_evaluate program =
    let (program_weight, childs) = StringMap.find program circus in
    StringSet.iter aux_evaluate childs;
    let childs_weight =
      StringSet.fold
        (fun elt acc ->
           let weigth = Hashtbl.find weight_table elt in
           weigth + acc)
        childs
        0
    in
    Hashtbl.replace weight_table program (program_weight + childs_weight)
  in
  aux_evaluate bottom;
  weight_table

let eval_diff circus weight_table bottom =
  (* Far to be the best solution possible... *)
  let rec aux_eval current_program =
    (* This solution is not adapted if we have to add weight to a program. *)
    let (_, childs) = StringMap.find current_program circus in
    (* Check if different weight exists. *)
    let elements =
      List.map
        (fun p -> (p, Hashtbl.find  weight_table p))
        (StringSet.elements childs)
      |> List.sort_uniq (fun (_, x) (_, y) -> compare y x)
    in
    match elements with
    | [ _ ] ->
      (* All elements are even. *)
      None
    | [ (program, high_weight); (_, low_weight) ] ->
      begin
        match aux_eval program with
        | None ->
          let diff = high_weight - low_weight in
          let (weight, _) = StringMap.find program circus in
          Some (weight - diff)
        | result ->
          result
      end
    | _ ->
      (* This should not happen... *)
      assert false
  in
  match aux_eval bottom with
  | None -> failwith "The input should not be even everywhere..."
  | Some result -> result

let solve input =
  let circus = fst input in
  let bottom = find_bottom_program input in
  let weight_table = evaluate_weight circus bottom in
  eval_diff circus weight_table bottom


(* INPUT *)

let input_line_circus line (circus, programs_set) =
  let scan_weight weight = Scanf.sscanf weight "(%d)" (fun i -> i) in
  let scan_child child = List.hd (String.split_on_char ',' child) in
  match String.split_on_char ' ' line with
  | [name; weight] ->
    let programs_set' = StringSet.add name programs_set in
    let weight = scan_weight weight in
    let circus' = StringMap.add name (weight, StringSet.empty) circus in
    circus', programs_set'
  | name :: weight :: "->" :: childs ->
    let programs_set' = StringSet.add name programs_set in
    let weight = scan_weight weight in
    let childs = StringSet.of_list (List.map scan_child childs) in
    let circus' = StringMap.add name (weight, childs) circus in
    circus', programs_set'
  | _ ->
    failwith (Format.sprintf "Bad input line: '%s'" line)

let () =
  let start = StringMap.empty, StringSet.empty in
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_all_lines ~start input_line_circus)
    ~aoc_solver:solve
    ~aoc_printer:string_of_int
