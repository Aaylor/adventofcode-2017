
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

let eval_input map =
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
  let result = aux_eval 0 ISet.empty map in
  ISet.cardinal result


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
