
(* The idea here is to sort all words from the line removing duplicates,
   and just check that the length of the two lists are still equal. *)

let is_valid_line line =
  let line = String.split_on_char ' ' line in
  let line_sorted = List.sort_uniq String.compare line in
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
