
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

let copy dancefloor =
  { floor = Array.copy dancefloor.floor;
    position = ProgramTbl.copy dancefloor.position }

let update dancefloor index program =
  dancefloor.floor.(index) <- program;
  ProgramTbl.replace dancefloor.position program index

let swap dancefloor (i1, p1) (i2, p2) =
  update dancefloor i1 p2;
  update dancefloor i2 p1

let dancefloor_to_string dancefloor =
  let buffer = Buffer.create 13 in
  Array.iter (Buffer.add_char buffer) dancefloor.floor;
  Buffer.contents buffer


(* MEMOIZATION *)

module MemoizationTbl = Hashtbl.Make(struct
    type t = int
    let equal = ( = )
    let hash = Hashtbl.hash
  end)


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

let find_fixpoint mem_table dancefloor moves =
  let first_dancefloor = copy dancefloor in
  let rec aux_find_fixpoint index =
    MemoizationTbl.replace mem_table index (copy dancefloor);
    List.iter (do_move dancefloor) moves;
    if dancefloor = first_dancefloor
    then succ index             (* This is the next index that is equal to the
                                   first dancefloor; not the current one.
                                   We need the exact index of the first equal
                                   dancefloor to retrieve the result in the
                                   memoization table. *)
    else aux_find_fixpoint (succ index)
  in
  aux_find_fixpoint 0

let do_dance moves times =
  let dancefloor = init_dancefloor () in
  let mem_table = MemoizationTbl.create 13 in
  let fixpoint_index = find_fixpoint mem_table dancefloor moves in
  MemoizationTbl.find
    mem_table
    (times mod fixpoint_index)

let dance moves =
  do_dance moves 1_000_000_000


(* INPUT *)

let () =
  let extract_moves l = List.map move_of_string (String.split_on_char ',' l) in
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_single_line extract_moves)
    ~aoc_solver:dance
    ~aoc_printer:dancefloor_to_string
