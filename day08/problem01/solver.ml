
(** Types  *)

type condition =
  | Condition of string * (int -> int -> bool) * int

type instruction =
  { register: string;
    operation: (int -> int -> int);
    value: int;
    condition: condition }


(** The interpreter *)

module Register: sig
  val fold: (string -> int -> 'c -> 'c) -> 'c -> 'c
  val set: string -> int -> unit
  val get: string -> int
end = struct
  let table = Hashtbl.create 13

  let fold fn v = Hashtbl.fold fn table v

  let set register value =
    Hashtbl.replace table register value

  let get register =
    match Hashtbl.find_opt table register with
    | None ->
      set register 0;
      0
    | Some value ->
      value
end

let maximum_value () =
  let result =
    Register.fold
      (fun _register value result ->
         match result with
         | None -> Some value
         | Some value' -> Some (max value value'))
      None
  in
  match result with
  | None -> failwith "Result can't be empty."
  | Some value -> value

let eval_condition (Condition (register, operator, value)) =
  let register_value = Register.get register in
  operator register_value value

let rec eval_instructions instructions =
  match instructions with
  | [] ->
    maximum_value ()
  | { register; operation; value; condition } :: instructions ->
    if eval_condition condition then begin
      let register_value = Register.get register in
      Register.set register (operation register_value value)
    end;
    eval_instructions instructions


(* INPUT *)

let parse_instruction line acc =
  let parse_operation = function
    | "inc" -> ( + )
    | "dec" -> ( - )
    | s -> failwith ("Unknown operation " ^ s)
  in
  let parse_operator = function
    | "<" -> ( < )
    | ">" -> ( > )
    | "<=" -> ( <= )
    | ">=" -> ( >= )
    | "==" -> ( = )
    | "!=" -> ( <> )
    | s -> failwith ("Unknown conditional operator " ^ s)
  in
  let do_parse register operation value cond_register operator cond_value =
    let operator = parse_operator operator in
    { register;
      operation = parse_operation operation;
      value;
      condition = Condition (cond_register, operator, cond_value) }
  in
  let instruction = Scanf.sscanf line "%s %s %d if %s %s %d" do_parse in
  instruction :: acc

let () =
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_all_lines_ordered ~start:[] parse_instruction)
    ~aoc_solver:eval_instructions
    ~aoc_printer:string_of_int
