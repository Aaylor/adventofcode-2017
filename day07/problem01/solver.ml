
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
    ~aoc_solver:find_bottom_program
    ~aoc_printer:(fun s -> s)
