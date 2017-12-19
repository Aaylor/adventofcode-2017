
(* Set & Map modules for int *)

module CamlInt = struct
  type t = int
  let compare = compare
end

module ISet = Set.Make(CamlInt)
module IMap = Map.Make(CamlInt)

let find_or_empty key map =
  match IMap.find_opt key map with
  | None -> ISet.empty
  | Some set -> set


(* Evaluation *)

let key_set map =
  IMap.fold (fun key _ acc -> ISet.add key acc) map ISet.empty

let eval_group current map =
  (* Evaluate each element of a group until they have been all reached at
     least once. *)
  let rec aux_eval current seen map =
    if ISet.mem current seen then
      seen
    else
      let to_eval = find_or_empty current map in
      ISet.fold
        (fun element acc ->
           aux_eval element acc map)
        to_eval
        (ISet.add current seen)
  in
  aux_eval current ISet.empty map

let eval_input map =
  let rec aux count set =
    if ISet.is_empty set then
      count
    else
      (* Evaluate the complete group, and remove all seen member
         in that group.
         When the set is empty, we have the number of process
         groups. *)
      let current = ISet.choose set in
      let set' = eval_group current map in
      let set' = ISet.diff set set' in
      aux (succ count) set'
  in
  aux 0 (key_set map)


(* INPUT *)

let with_channel filename fn =
  let channel = open_in filename in
  try
    let result = fn channel in
    close_in channel;
    result
  with exn ->
    close_in channel;
    raise exn

let extract_value value =
  int_of_string (List.hd (String.split_on_char ',' value))

let process_input channel =
  let rec read_line map =
    try
      let line = input_line channel in
      match String.split_on_char ' ' line with
      | key :: "<->" :: values ->
        let key = int_of_string key in
        let set' =
          List.fold_left
            (fun acc value ->
               let value = extract_value value in
               ISet.add value acc)
            (find_or_empty key map)
            values
        in
        read_line (IMap.add key set' map)
      | _ ->
        failwith "Invalid line"
    with End_of_file ->
      map
  in
  read_line IMap.empty

let () =
  let input_map = with_channel "input" process_input in
  let result = eval_input input_map in
  Format.printf "%d@." result
