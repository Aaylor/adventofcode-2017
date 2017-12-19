
module StringSet = Set.Make(String)

let circus: (string, (int * StringSet.t)) Hashtbl.t = Hashtbl.create 13
let program_set: StringSet.t ref = ref StringSet.empty

let find_bottom_program () =
  let childs =
    Hashtbl.fold
      (fun _ (_, set) acc -> StringSet.union set acc)
      circus
      StringSet.empty
  in
  let result = StringSet.diff !program_set childs in
  StringSet.choose result


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

let input_circus channel =
  let scan_weight weight = Scanf.sscanf weight "(%d)" (fun i -> i) in
  let scan_child child = List.hd (String.split_on_char ',' child) in
  let register_program p = program_set := StringSet.add p !program_set in
  let process_line line =
    match String.split_on_char ' ' line with
    | [name; weight] ->
      register_program name;
      let weight = scan_weight weight in
      Hashtbl.add circus name (weight, StringSet.empty)
    | name :: weight :: "->" :: childs ->
      register_program name;
      let weight = scan_weight weight in
      let childs = List.map scan_child childs in
      Hashtbl.add circus name (weight, StringSet.of_list childs)
    | _ ->
      failwith (Format.sprintf "Bad input line: '%s'" line)
  in
  let rec lines () =
    try
      let line = input_line channel in
      process_line line;
      lines ()
    with End_of_file ->
      ()
  in
  lines ()

let () =
  let () = with_channel "input" input_circus in
  let result = find_bottom_program () in
  Format.printf "%s@." result
