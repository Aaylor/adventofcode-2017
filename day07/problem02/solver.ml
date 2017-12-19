
module StringSet = Set.Make(String)

let circus: (string, (int * StringSet.t)) Hashtbl.t = Hashtbl.create 13
let table_weight: (string, int) Hashtbl.t = Hashtbl.create 13
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

let evaluate_weight bottom =
  let rec aux_evaluate program =
    let (program_weight, childs) = Hashtbl.find circus program in
    StringSet.iter aux_evaluate childs;
    let childs_weight =
      StringSet.fold
        (fun elt acc ->
           let weigth = Hashtbl.find table_weight elt in
           weigth + acc)
        childs
        0
    in
    Hashtbl.replace table_weight program (program_weight + childs_weight)
  in
  aux_evaluate bottom

let eval_diff bottom =
  (* Far to be the best solution possible... *)
  let rec aux_eval current_program =
    (* This solution is not adapted if we have to add weight to a program. *)
    let (_, childs) = Hashtbl.find circus current_program in
    (* Check if different weight exists. *)
    let elements =
      List.map
        (fun p -> (p, Hashtbl.find table_weight p))
        (StringSet.elements childs)
      |> List.sort_uniq (fun (_, x) (_, y) -> compare y x)
    in
    match elements with
    | [ _ ] ->
      (* All elements are even. *)
      None
    | [ (program, high_weight); (_, low_weight) ] ->
      begin
        match aux_eval program with
        | None ->
          let diff = high_weight - low_weight in
          let (weight, _) = Hashtbl.find circus program in
          Some (weight - diff)
        | result ->
          result
      end
    | _ ->
      (* This should not happen... *)
      assert false
  in
  match aux_eval bottom with
  | None -> failwith "The input should not be even everywhere..."
  | Some result -> result


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
  let bottom = find_bottom_program () in
  evaluate_weight bottom;
  Format.printf "%d@." (eval_diff bottom)
