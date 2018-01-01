
(* TYPES *)

type register = string

type rvalue =
  | Register of register
  | Int of int

type instruction =
  | Send of rvalue
  | Set of register * rvalue
  | Add of register * rvalue
  | Mul of register * rvalue
  | Mod of register * rvalue
  | Recover of register
  | Jump of rvalue * rvalue


(* REGISTERS *)

module Register: sig
  include Map.S with type key = string
  val find_default: default:'a -> key -> 'a t -> 'a
end = struct
  include Map.Make(String)
  let find_default ~default key table =
    match find_opt key table with
    | None -> default
    | Some value -> value
end


(* DUET *)

let get_register_value context register =
  Register.find_default ~default:0 register context

let eval_rvalue context rvalue =
  match rvalue with
  | Int i -> i
  | Register register -> get_register_value context register

let eval_operation context op register rvalue =
  let register_value = get_register_value context register in
  let rvalue_value = eval_rvalue context rvalue in
  Register.add register (op register_value rvalue_value) context

let start_evaluation ~pid instructions command =
  let rec aux_duet ~stack ~queue context index =
    match instructions.(index) with
    | Send rvalue ->
      let register_value = eval_rvalue context rvalue in
      aux_duet ~stack:(register_value :: stack) ~queue context (succ index)
    | Set (register, rvalue) ->
      let rvalue = eval_rvalue context rvalue in
      let context' = Register.add register rvalue context in
      aux_duet ~stack ~queue context' (succ index)
    | Add (register, rvalue) ->
      let context' = eval_operation context ( + ) register rvalue in
      aux_duet ~stack ~queue context' (succ index)
    | Mul (register, rvalue) ->
      let context' = eval_operation context ( * ) register rvalue in
      aux_duet ~stack ~queue context' (succ index)
    | Mod (register, rvalue) ->
      let context' = eval_operation context ( mod ) register rvalue in
      aux_duet ~stack ~queue context' (succ index)
    | Recover register ->
      begin
        match queue with
        | [] ->
          `Waiting (index, context, List.rev stack)
        | x :: xs ->
          let context' = Register.add register x context in
          aux_duet ~stack ~queue:xs context' (succ index)
      end
    | Jump (register, rvalue) ->
      let register_value = eval_rvalue context register in
      let index =
        if register_value > 0 then index + (eval_rvalue context rvalue)
        else succ index
      in
      aux_duet ~stack ~queue context index
  in
  match command with
  | `Starting ->
    let context = Register.singleton "p" pid in
    aux_duet ~stack:[] ~queue:[] context 0
  | `Resume (index, context, queue) ->
    aux_duet ~stack:[] ~queue context index

let rec eval_duet ~sent instructions p1 p2 =
  match p1, p2 with
  | `Waiting (p1_index, p1_context, []),
    `Waiting (p2_index, p2_context, []) ->
    sent
  | `Waiting (p1_index, p1_context, p1_stack),
    `Waiting (p2_index, p2_context, p2_stack) ->
    let sent = sent + List.length p2_stack in
    (* Creates command to resume both programs *)
    let p1_command = `Resume (p1_index, p1_context, p2_stack) in
    let p2_command = `Resume (p2_index, p2_context, p1_stack) in
    (* Then restart them *)
    let p1' = start_evaluation ~pid:0 instructions p1_command in
    let p2' = start_evaluation ~pid:1 instructions p2_command in
    (* Eval the result again *)
    eval_duet ~sent instructions p1' p2'

let do_duet instructions =
  let p1 = start_evaluation ~pid:0 instructions `Starting in
  let p2 = start_evaluation ~pid:1 instructions `Starting in
  eval_duet ~sent:0 instructions p1 p2


(* INPUT *)

let parse_instruction line acc =
  let parse_rvalue rvalue =
    try Int (int_of_string rvalue)
    with Failure _ -> Register rvalue
  in
  let instruction =
    match String.split_on_char ' ' line with
    | [ "snd"; rvalue ] ->
      Send (parse_rvalue rvalue)
    | [ "rcv"; register ] ->
      Recover register
    | [ "set"; register; rvalue ] ->
      Set (register, parse_rvalue rvalue)
    | [ "add"; register; rvalue ] ->
      Add (register, parse_rvalue rvalue)
    | [ "mul"; register; rvalue ] ->
      Mul (register, parse_rvalue rvalue)
    | [ "mod"; register; rvalue ] ->
      Mod (register, parse_rvalue rvalue)
    | [ "jgz"; rvalue; rvalue' ] ->
      Jump (parse_rvalue rvalue, parse_rvalue rvalue')
    | _ ->
      failwith ("Unknonw instruction: " ^ line)
  in
  instruction :: acc

let () =
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_all_lines_ordered ~start:[] parse_instruction)
    ~aoc_solver:(fun is -> do_duet (Array.of_list is))
    ~aoc_printer:string_of_int
