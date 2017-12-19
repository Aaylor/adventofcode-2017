
module ExtString = struct
  let get_opt str idx =
    try Some (String.get str idx)
    with Invalid_argument _ -> None

  let foldi fn acc input =
    let rec aux ~acc ~index =
      match get_opt input index with
      | None ->
        acc
      | Some c ->
        let acc = fn index acc c in
        aux ~acc ~index:(succ index)
    in
    aux ~acc ~index:0
end

let int_value char =
  let code = Char.code char in
  if code < 48 && code > 57
  then failwith (Format.sprintf "The character '%c' is not a digit." char)
  else code - 48

let eval input =
  let eval_char index acc current_char =
    let next_char =
      match ExtString.get_opt input (succ index) with
      | None -> String.get input 0
      | Some c -> c
    in
    if current_char = next_char
    then acc + (int_value current_char)
    else acc
  in
  ExtString.foldi eval_char 0 input

let read_input filename =
  let channel = open_in filename in
  let input =
    try
      input_line channel
    with
    | End_of_file ->
      Format.eprintf "The input file must have at least one line@.";
      close_in channel;
      exit 1
  in
  close_in channel;
  input

let () =
  let input = read_input "input" in
  let result = eval input in
  Format.printf "%d@." result
