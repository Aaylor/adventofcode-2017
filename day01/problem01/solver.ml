
let int_value char =
  let code = Char.code char in
  if code < 48 && code > 57
  then failwith (Format.sprintf "The character '%c' is not a digit." char)
  else code - 48

let eval input =
  let eval_char index acc current_char =
    let next_char =
      match Aoc_lib.ExtString.get_opt input (succ index) with
      | None -> String.get input 0
      | Some c -> c
    in
    if current_char = next_char
    then acc + (int_value current_char)
    else acc
  in
  Aoc_lib.ExtString.foldi eval_char 0 input


(* INPUT *)

let () =
  Aoc_solver.solve
    ~aoc_parser:Aoc_solver.parser_single_string_line
    ~aoc_solver:eval
    ~aoc_printer:string_of_int
