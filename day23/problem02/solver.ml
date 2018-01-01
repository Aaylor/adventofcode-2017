
(* For that we just translate to CAML and then do optimization on what does
   the assembly code do. *)

(* We do the supposition that the register 'a' is 1 and not 0.
   So, we do the initialization of b and c considering a <> 0. *)

let input_b = 93 * 100 + 100_000
let input_c = input_b + 17_000

let find_first_higher_square_root =
  (* We assume that [n] is always growing. So we just keep track
     of the last returned value, and use it as the first index to
     use. *)
  let last_index = ref 2 in
  fun n ->
    let rec aux_find_first_higher_square_root index =
      if index * index <= n then
        aux_find_first_higher_square_root (succ index)
      else begin
        last_index := index;
        index
      end
    in
    aux_find_first_higher_square_root !last_index

let solve () =
  let rec aux_count_of_prime_numbers count input_b =
    if input_b > input_c then
      count
    else if input_b mod 2 = 0 then
      aux_count_of_prime_numbers (succ count) (input_b + 17)
    else
      let square_root = find_first_higher_square_root input_b in
      let rec loop_j j =
        let rec loop_k k =
          k < input_b &&
          (j * k = input_b || loop_k (k + 2))
        in
        j < square_root &&
        (loop_k j || loop_j (j + 2))
      in
      let count = if loop_j 3 then (succ count) else count in
      aux_count_of_prime_numbers count (input_b + 17)
  in
  aux_count_of_prime_numbers 0 input_b


(* INPUT *)

let () =
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_static ())
    ~aoc_solver:solve
    ~aoc_printer:string_of_int
