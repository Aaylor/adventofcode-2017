
(* SPINLOCK *)

let rec insert_at nth l v =
  match l with
  | _ when nth = 0 -> v :: l
  | [] when nth <> 0 -> failwith "bad insertion"
  | [] -> [v]
  | x :: xs -> x :: (insert_at (pred nth) xs v)

let do_spinlock values spin =
  let rec aux_spinlock ~index ~position ~result =
    if index >= values then
      result
    else
      let position = (position + spin) mod index in
      (* The insertion here should have been smarter here.
         Instead of running the list from the beginning every time,
         we should have start from the last position if the new
         one is < length.
         But laziness is laziness \o/ *)
      let result = insert_at (position + 1) result index in
      aux_spinlock ~index:(succ index) ~position:(position + 1) ~result
  in
  aux_spinlock ~index:1 ~position:0 ~result:[ 0 ]

let rec find_result spinlock values =
  match spinlock with
  | [] ->
    failwith "the result cannot be the empty list"
  | [ x ] ->
    if x = (values - 1) then List.hd spinlock
    else failwith "the searched value does not exist"
  | x :: y :: spinlock ->
    if x = (values - 1) then y
    else find_result (y :: spinlock) values

let solve values input =
  let spinlock = do_spinlock values input in
  find_result spinlock values


(* INPUT *)

let values = 2018
let input = 369

let () =
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_static input)
    ~aoc_solver:(solve values)
    ~aoc_printer:string_of_int
