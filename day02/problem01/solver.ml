
let do_check op element input =
  match element with
  | None -> Some input
  | Some element -> Some (if op element input then element else input)

let check_min min input = do_check ( < ) min input
let check_max max input = do_check ( > ) max input

let eval matrix =
  let eval_row (min, max) element =
    (** For each element update the minimum and the maximum element of
        the list. *)
    let min' = check_min min element in
    let max' = check_max max element in
    min', max'
  in
  let eval_rows sum row =
    let row_result = List.fold_left eval_row (None, None) row in
    match row_result with
    | Some min, Some max -> sum + (max - min)
    | _ -> failwith "bad input"
  in
  List.fold_left eval_rows 0 matrix


(* INPUT *)

let parse_line line acc =
  (* It assume that the given input has its element separated by
     \t characters, and not spaces. *)
  let result_line =
    List.map
      int_of_string
      (String.split_on_char '\t' line)
  in
  result_line :: acc

let () =
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_all_lines ~start:[] parse_line)
    ~aoc_solver:eval
    ~aoc_printer:string_of_int
