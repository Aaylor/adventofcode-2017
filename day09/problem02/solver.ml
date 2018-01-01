
let eval input =
  let rec eval_group ~index depth sum =
    let index' = succ index in
    match input.[index] with
    | '{' -> eval_group ~index:index' (succ depth) sum
    | ',' -> eval_group ~index:index' depth sum
    | '}' ->
      (* We assume that the input always start with '{'. Then, closing
         the first bracket would lead to depth equal to 0, which is
         the end of input.
         We make the assertion that the input cannot contain mutliple
         "toplevel" groups. *)
      if depth = 1 then sum
      else eval_group ~index:index' (pred depth) sum
    | '<' ->
      garbage ~index:index' depth sum
    | _ ->
      failwith "Malformed input"
  and garbage ~index depth sum =
    let index' = succ index in
    match input.[index]  with
    | '>' ->
      eval_group ~index:index' depth sum
    | '!' ->
      garbage ~index:(succ index') depth sum
    | _ ->
      garbage ~index:index' depth (succ sum)
  in
  eval_group ~index:0 0 0


(* INPUT *)

let () =
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_single_line (fun s -> s))
    ~aoc_solver:eval
    ~aoc_printer:string_of_int
