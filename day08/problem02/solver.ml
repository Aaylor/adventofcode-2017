

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
  val maximum_value_stored: unit -> int
  val set: string -> int -> unit
  val get: string -> int
end = struct
  let table = Hashtbl.create 13

  let maximum_value = ref None

  let set_maximum_value_stored value =
    match !maximum_value with
    | None -> maximum_value := Some value
    | Some value' -> if value > value' then maximum_value := Some value

  let maximum_value_stored () =
    match !maximum_value with
    | None -> failwith "Maximum value stored can't be empty"
    | Some value -> value

  let set register value =
    set_maximum_value_stored value;
    Hashtbl.replace table register value

  let get register =
    match Hashtbl.find_opt table register with
    | None ->
      set register 0;
      0
    | Some value ->
      value
end

let eval_condition (Condition (register, operator, value)) =
  let register_value = Register.get register in
  operator register_value value

let rec eval_instructions instructions =
  match Stream.peek instructions with
  | None ->
    Register.maximum_value_stored ()
  | Some { register; operation; value; condition } ->
    ignore (Stream.next instructions);
    if eval_condition condition then begin
      let register_value = Register.get register in
      Register.set register (operation register_value value)
    end;
    eval_instructions instructions


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

let parse_instruction line =
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
  Scanf.sscanf line "%s %s %d if %s %s %d" do_parse

let stream_instruction channel =
  let read_line _ =
    try Some (parse_instruction (input_line channel))
    with End_of_file -> None
  in
  Stream.from read_line

let () =
  with_channel "input"
    (fun channel ->
       let instructions = stream_instruction channel in
       let result = eval_instructions instructions in
       Format.printf "%d@." result)
