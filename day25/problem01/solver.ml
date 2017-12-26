
(** {2 Values} *)

(** The representation of value a cell can contains. *)
type value =
  | Zero
  | One

let value_of_int i =
  match i with
  | 0 -> Zero
  | 1 -> One
  | _ -> failwith ("Invalid value: " ^ string_of_int i)


(** {2 Move} *)

(** The representation of movement we can make on the tape. *)
type move =
  | Left
  | Right

let move_of_string s =
  match s with
  | "left" -> Left
  | "right" -> Right
  | _ -> failwith ("Invalid move: " ^ s)


(**  {2 Rules} *)

(** The rules that can be applied by each states. *)
type rule =
  | Write of value
  (** Write the [value] at the current cursor. *)
  | Move of int * move
  (** Move the cursor by [n] steps to the Left or to the Right. *)
  | NextState of string
  (** Jumping to the next given state. *)


(** {2 Contexts} *)

module ValueMap = Map.Make(struct
    type t = value
    let compare = compare
  end)

module TapeMap = Map.Make(struct
    type t = int
    let compare = compare
  end)

module StateMap = Map.Make(String)

type context =
  { states: rule list ValueMap.t StateMap.t;
    (** The rules map for each state.
        Each state has two different rule list: one when the value is
        [Zero], and another when the value is [One]. *)
    values: value TapeMap.t;
    (** Cursor indexes mapped to values. *)
    state: string;
    (** The current state to run.  *)
    cursor: int;
    (** The tape cursor.  *)
    steps: int;
    (** The number of steps done. *) }


(** {2 Run the tape} *)

let read_value context =
  match TapeMap.find_opt context.cursor context.values with
  | None -> Zero
  | Some value -> value

let apply_rule context rule =
  match rule with
  | Write value ->
    let values = TapeMap.add context.cursor value context.values in
    { context with values; }
  | Move (steps, move) ->
    let direction = if move = Left then -1 else 1 in
    { context with
      cursor = context.cursor + (steps * direction)}
  | NextState state ->
    { context with state }

let run context =
  let rec aux_run step context =
    if step >= context.steps then
      context
    else
      let current_value = read_value context in
      let state_map = StateMap.find context.state context.states in
      let rules = ValueMap.find current_value state_map in
      let context' = List.fold_left apply_rule context rules in
      aux_run (succ step) context'
  in
  let context_end = aux_run 0 context in
  TapeMap.fold
    (fun _ v acc -> if v = Zero then acc else succ acc)
    context_end.values
    0


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

let with_line channel fn = fn (String.trim (input_line channel))

let with_fmt_line channel fmt =
  let identity c = c in
  with_line channel (fun s -> Scanf.sscanf s fmt identity)

let parse_state_condition value_map channel =
  let value = with_fmt_line channel "If the current value is %d:" in
  let write = with_fmt_line channel "- Write the value %d." in
  let move = with_fmt_line channel "- Move one slot to the %s@." in
  let next = with_fmt_line channel "- Continue with state %s@." in
  ValueMap.add
    (value_of_int value)
    [ Write (value_of_int write);
      Move (1, move_of_string move);
      NextState next ]
    value_map

let rec parse_states map channel =
  try
    ignore (input_line channel); (* Read an empty line before parsing the
                                    state. *)
    let state = with_fmt_line channel "In state %s@:" in
    let zero_state = parse_state_condition ValueMap.empty channel in
    let one_state = parse_state_condition zero_state channel in
    let map' = StateMap.add state one_state map in
    parse_states map' channel
  with End_of_file ->
    map

let parse_tape channel =
  let begin_state = with_fmt_line channel "Begin in state %s@." in
  let steps =
    with_fmt_line channel
      "Perform a diagnostic checksum after %d steps."
  in
  let states = parse_states StateMap.empty channel in
  { state = begin_state; steps; states; cursor = 0; values = TapeMap.empty;  }

let () =
  let context = with_channel "input" parse_tape in
  let diagnostic = run context in
  Format.printf "%d@." diagnostic
