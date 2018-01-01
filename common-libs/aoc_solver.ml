
exception Aoc_parser_failed of string


type 'input aoc_parser =
  | Static: 'input -> 'input aoc_parser
  | SingleLine: (string -> 'input) -> 'input aoc_parser
  | AllLines: 'input * (string -> 'input -> 'input) -> 'input aoc_parser
  | OrderedAllLines: 'input * (string -> 'input -> 'input) -> 'input aoc_parser
  | Custom: (in_channel -> 'input) -> 'input aoc_parser

let parser_static input =
  Static input

let parser_single_line wrapper =
  SingleLine wrapper

let parser_single_string_line =
  SingleLine (fun s -> s)

let parser_single_int_line =
  SingleLine int_of_string

let parser_all_lines ~start callback =
  AllLines (start, callback)

let parser_all_lines_ordered ~start callback =
  OrderedAllLines (start, callback)

let parser_custom callback =
  Custom callback


let with_channel filename fn =
  let channel = open_in filename in
  try
    let result = fn channel in
    close_in channel;
    result
  with exn ->
    close_in channel;
    raise exn

let one_line callback channel =
  try callback (input_line channel)
  with End_of_file -> raise (Aoc_parser_failed "TODO")

let all_lines acc fn channel =
  let rec lines acc =
    try
      let line = input_line channel in
      let acc' = fn line acc in
      lines acc'
    with End_of_file ->
      acc
  in
  lines acc

let ordered_all_lines acc fn channel =
  let rec lines acc =
    try
      let line = input_line channel in
      let acc' = lines acc in
      let acc' = fn line acc' in
      lines acc'
    with End_of_file ->
      acc
  in
  lines acc

type ('input, 'output) aoc_solver = 'input -> 'output
type 'output aoc_printer = 'output -> string

let solve ~aoc_parser ~aoc_solver ~aoc_printer =
  let start_time = Unix.gettimeofday () in
  let input =
    match aoc_parser with
    | Static i -> i
    | SingleLine foo -> with_channel "input" (one_line foo)
    | AllLines (start, callback) ->
      with_channel "input" (all_lines start callback)
    | OrderedAllLines (start, callback) ->
      with_channel "input" (ordered_all_lines start callback)
    | Custom callback ->
      with_channel "input" callback
  in
  let result = aoc_solver input in
  let end_time = Unix.gettimeofday () in
  Format.printf "Execution time: %f@." ((end_time -. start_time) *. 1000.);
  Format.printf "%s@." (aoc_printer result)
