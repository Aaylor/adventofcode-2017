
(* Move types *)

type move =
  | Spin of int
  | Exchange of int * int
  | Partner of char * char

let move_of_string s =
  match s.[0] with
  | 's' -> Scanf.sscanf s "s%d" (fun d -> Spin d)
  | 'x' -> Scanf.sscanf s "x%d/%d" (fun i j -> Exchange (i, j))
  | 'p' -> Scanf.sscanf s "p%c/%c" (fun c1 c2 -> Partner (c1, c2))
  | _ -> failwith "Unknown move character"


(* Program types *)

module ProgramTbl = Hashtbl.Make(struct
    type t = char
    let equal = Char.equal
    let hash = Hashtbl.hash
  end)

type dancefloor =
  { floor: char array;
    position: int ProgramTbl.t }

let init_dancefloor () =
  let number = 16 in
  let position = ProgramTbl.create number in
  let do_init i =
    let program = Char.chr (Char.code 'a' + i) in
    ProgramTbl.replace position program i;
    program
  in
  let floor = Array.init number do_init in
  { floor; position }

let update dancefloor index program =
  dancefloor.floor.(index) <- program;
  ProgramTbl.replace dancefloor.position program index

let swap dancefloor (i1, p1) (i2, p2) =
  update dancefloor i1 p2;
  update dancefloor i2 p1

let print_dancefloor dancefloor =
  Array.iter print_char dancefloor.floor;
  Format.printf "@."


(* DANSE *)

let do_spin dancefloor spin_size =
  let length = Array.length dancefloor.floor in
  Array.iteri
    (fun index program ->
       let index' = (index + spin_size) mod length in
       update dancefloor index' program)
    (Array.copy dancefloor.floor)

let do_move dancefloor move =
  match move with
  | Spin i ->
    do_spin dancefloor i
  | Exchange (i1, i2) ->
    let p1 = dancefloor.floor.(i1) in
    let p2 = dancefloor.floor.(i2) in
    swap dancefloor (i1, p1) (i2, p2)
  | Partner (p1, p2) ->
    let i1 = ProgramTbl.find dancefloor.position p1 in
    let i2 = ProgramTbl.find dancefloor.position p2 in
    swap dancefloor (i1, p1) (i2, p2)

let danse moves =
  let dancefloor = init_dancefloor () in
  List.iter (do_move dancefloor) moves;
  dancefloor


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

let extract_moves channel =
  try
    let line = input_line channel in
    let moves = String.split_on_char ',' line in
    List.map move_of_string moves
  with End_of_file ->
    failwith "Input must have at least one line"

let () =
  let moves = with_channel "input" extract_moves in
  let dancefloor = danse moves in
  print_dancefloor dancefloor
