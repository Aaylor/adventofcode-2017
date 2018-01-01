
(* TYPES *)

type register = string

type rvalue =
  | Register of register
  | Int of int

type instruction =
  | Sound of rvalue
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

let do_duet instructions =
  let rec aux_duet ~last_sound context index =
    match instructions.(index) with
    | Sound register ->
      let register_value = eval_rvalue context register in
      aux_duet ~last_sound:(Some register_value) context (succ index)
    | Set (register, rvalue) ->
      let rvalue = eval_rvalue context rvalue in
      let context' = Register.add register rvalue context in
      aux_duet ~last_sound context' (succ index)
    | Add (register, rvalue) ->
      let context' = eval_operation context ( + ) register rvalue in
      aux_duet ~last_sound context' (succ index)
    | Mul (register, rvalue) ->
      let context' = eval_operation context ( * ) register rvalue in
      aux_duet ~last_sound context' (succ index)
    | Mod (register, rvalue) ->
      let context' = eval_operation context ( mod ) register rvalue in
      aux_duet ~last_sound context' (succ index)
    | Recover register ->
      let register_value = get_register_value context register in
      if register_value <> 0 then last_sound
      else aux_duet ~last_sound context (succ index)
    | Jump (register, rvalue) ->
      let register_value = eval_rvalue context register in
      let index =
        if register_value > 0 then index + (eval_rvalue context rvalue)
        else succ index
      in
      aux_duet ~last_sound context index
  in
  match aux_duet ~last_sound:None Register.empty 0 with
  | None -> failwith "no sound played"
  | Some sound -> sound


(* INPUT *)

let parse_instruction line acc =
  let parse_rvalue rvalue =
    try Int (int_of_string rvalue)
    with Failure _ -> Register rvalue
  in
  let instruction =
    match String.split_on_char ' ' line with
    | [ "snd"; rvalue ] ->
      Sound (parse_rvalue rvalue)
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
