
(* The bottom right of each *square* has always the value (size ^ 2), where size
   is the size of one side of the square.

   The square cannot have a even size; they're always odd (1, 3, 5, ...).

   To solve the problem, we use the following solution:
     - find the closest square root of the input; if it's even, add one.
     - find the axis of the square where the input does come from.
     - find the number of steps to reach the input from one of the four
       center of the square.
     - add the axis to the number of steps from the center to get the result. *)

let closest_square_root input =
  (* This returns the size of one side of the square *)
  let as_float = float_of_int input in
  let result = int_of_float (ceil (sqrt as_float)) in
  (* The square cannot have the size its side even. So add one if the
     result is even. *)
  if result mod 2 = 0 then result + 1 else result

let axis_to_center square_root =
  (* It returns the number of steps to reach the axis
     from the center. *)
  (square_root - 1) / 2

let steps_from_center input square_root steps_to_axis =
  (* This function returns the number of steps to do to reach the input
     from the center of one of the square side.

     init_value: this is the value of the cell at the bottom right of the
     square. Every centered cell will be calculcated from this cell. *)
  let init_value = square_root * square_root in
  let do_calc n = abs (input - (init_value - (n * steps_to_axis))) in
  do_calc 1 |>
  min (do_calc 3) |>
  min (do_calc 5) |>
  min (do_calc 7)


(* Inputs *)

let input = 361527

let () =
  let square_root = closest_square_root input in
  let axis = axis_to_center square_root in
  let steps_from_center = steps_from_center input square_root axis in
  (* The final number correspond to the number of steps to reach the
     center of the correct axis, and then the number of steps to reach
     the input from the calculated center. *)
  Format.printf "%d@." (axis + steps_from_center)
