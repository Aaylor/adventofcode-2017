
type generators =
  { a: Int64.t;
    b: Int64.t; }

let divider = Int64.of_string "2147483647"
let max_uint16 = Int64.of_int 0xFFFF

let next_value factor value = Int64.rem (Int64.mul value factor) divider
let next_a = next_value (Int64.of_int 16807)
let next_b = next_value (Int64.of_int 48271)

let rec find_next fn_next modulo value =
  let next_value = fn_next value in
  if Int64.rem next_value modulo = Int64.zero
  then next_value
  else find_next fn_next modulo next_value
let find_next_a = find_next next_a (Int64.of_int 4)
let find_next_b = find_next next_b (Int64.of_int 8)

let next_generator generator =
  { a = find_next_a generator.a;
    b = find_next_b generator.b }

let last_16_bits int64 =
  Int64.logand int64 max_uint16

let judge generator =
  let rec loop ~generator ~index ~count =
    if index > 0 then begin
      let generator' = next_generator generator in
      let count =
        if Int64.equal (last_16_bits generator'.a) (last_16_bits generator'.b)
        then succ count
        else count
      in
      loop ~generator:generator' ~index:(pred index) ~count
    end else
      count
  in
  let index = 5_000_000 in
  loop ~generator ~index ~count:0


(* INPUT *)

let extract_input channel =
  try
    let line1 = input_line channel in
    let line2 = input_line channel in
    let int64 i = Int64.of_int i in
    let a = Scanf.sscanf line1 "Generator A starts with %d" int64 in
    let b = Scanf.sscanf line2 "Generator B starts with %d" int64 in
    { a; b }
  with End_of_file ->
    failwith "The file should contain at least 2 lines."

let () =
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_custom extract_input)
    ~aoc_solver:judge
    ~aoc_printer:string_of_int
