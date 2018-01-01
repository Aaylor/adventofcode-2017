
type 'input aoc_parser
type ('input, 'output) aoc_solver = 'input -> 'output
type 'output aoc_printer = 'output -> string

val parser_static: 'a -> 'a aoc_parser
val parser_single_line: (string -> 'a) -> 'a aoc_parser
val parser_single_string_line: string aoc_parser
val parser_single_int_line: int aoc_parser

(*  *)
val parser_all_lines: start:'a -> (string -> 'a -> 'a) -> 'a aoc_parser
val parser_all_lines_ordered: start:'a -> (string -> 'a -> 'a) -> 'a aoc_parser


val solve:
  aoc_parser:'input aoc_parser ->
  aoc_solver:('input, 'output) aoc_solver ->
  aoc_printer:'output aoc_printer ->
  unit
