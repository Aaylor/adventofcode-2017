
let eval stream =
  let rec eval_group depth sum =
    match Stream.next stream with
    | '{' -> eval_group (succ depth) sum
    | ',' -> eval_group depth sum
    | '}' ->
      (* We assume that the input always start with '{'. Then, closing
         the first bracket would lead to depth equal to 0, which is
         the end of input.
         We make the assertion that the input cannot contain mutliple
         "toplevel" groups. *)
      if depth = 1 then sum
      else eval_group (pred depth) sum
    | '<' ->
      garbage depth sum
    | _ ->
      failwith "Malformed input"
  and garbage depth sum =
    match Stream.next stream with
    | '>' ->
      eval_group depth sum
    | '!' ->
      ignore (Stream.next stream);
      garbage depth sum
    | _ ->
      garbage depth (succ sum)
  in
  eval_group 0 0


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

let () =
  let result =
    with_channel "input"
      (fun channel ->
         let stream = Stream.of_channel channel in
         eval stream)
  in
  (* let result = *)
  (*   let s = "{<random characters>}" in *)
  (*   eval (Stream.of_string s) *)
  (*   in *)
  Format.printf "%d@." result
