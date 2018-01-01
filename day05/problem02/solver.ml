
let process_instructions instructions =
  let lower_bound, higher_bound = 0, Array.length instructions - 1 in
  let rec do_process steps idx =
    if idx < lower_bound || idx > higher_bound
    then steps
    else
      let value = instructions.(idx) in
      let next_idx = idx + value in
      instructions.(idx) <- if value >= 3 then value - 1 else value + 1;
      do_process (succ steps) next_idx
  in
  do_process 0 0


(* INPUT *)

let () =
  let transform line acc = int_of_string line :: acc in
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_all_lines_ordered ~start:[] transform)
    ~aoc_solver:(fun is -> process_instructions (Array.of_list is))
    ~aoc_printer:string_of_int
