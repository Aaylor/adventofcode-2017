
(* SPINLOCK *)

let do_spinlock values spin =
  (* Since 0 always stays at the first position, we just have to look at
     insertion at index the position right after 0 (1). *)
  let rec aux_spinlock ~index ~position ~result =
    if index >= values
    then result
    else
      let position = (position + spin) mod index in
      let result = if position = 0 then index else result in
      aux_spinlock ~index:(succ index) ~position:(position + 1) ~result
  in
  aux_spinlock ~index:1 ~position:0 ~result:(-1)


(* INPUT *)

let values = 50_000_001
let input = 369

let () =
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_static input)
    ~aoc_solver:(do_spinlock values)
    ~aoc_printer:string_of_int
