
(* The idea here is to sort all letters of all words of the line; and
   then to check if there is a duplicate word in the result list by checking
   the size of the two list *)

let to_sorted_char_list word =
  let rec explode idx acc =
    if idx < 0
    then acc
    else explode (pred idx) (word.[idx] :: acc)
  in
  let char_list = explode (String.length word - 1) [] in
  List.sort compare char_list

let is_valid_line line =
  let line = String.split_on_char ' ' line in
  let line = List.map to_sorted_char_list line in
  let line_sorted = List.sort_uniq compare line in
  List.compare_lengths line line_sorted = 0


(* INPUT *)

let check_line line valid_lines =
  if is_valid_line line then succ valid_lines
  else valid_lines

let () =
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_all_lines ~start:0 check_line)
    ~aoc_solver:(fun i -> i)
    ~aoc_printer:string_of_int
