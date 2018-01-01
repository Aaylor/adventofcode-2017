
(* TYPES *)

type port =
  { v1: int;
    v2: int; }

module PortSet = Set.Make(struct
    type t = port
    let compare = compare
  end)

module IntMap = Map.Make(struct
    type t  = int
    let compare = compare
  end)


(* BRIDGES *)

let pin_to_use last_pin port =
  assert (last_pin = port.v1 || last_pin = port.v2);
  if last_pin = port.v1 then port.v2
  else port.v1

let port_sum port =
  port.v1 + port.v2

let rec eval_bridge ~size last_pin port map seen =
  let pin = pin_to_use last_pin port in
  let port_strength = port_sum port in
  match IntMap.find_opt pin map with
  | None ->
    failwith "The input must containt the pin."
  | Some set ->
    (* If the node has been seen, ensure that we won't loop on it.  *)
    let set' = PortSet.diff set seen in
    if PortSet.is_empty set' then
      (* When there is no port child possible, we just returns the sum of this
         branch and let the root branch handle this result. *)
      size, port_sum port
    else
      PortSet.fold
        (fun element (size_acc, strength_acc) ->
           let seen' = PortSet.add element seen in
           let size, strength =
             eval_bridge ~size:(succ size)
               pin element map seen'
           in
           let strength = strength + port_strength in
           if size_acc > size then (size_acc, strength_acc)
           else if size_acc = size then (size_acc, max strength strength_acc)
           else (size, strength))
        set' (size, 0)

let start_bridge map =
  (* We start from zero-pin ports. *)
  match IntMap.find_opt 0 map with
  | None ->
    failwith "The input must contain at least one 0-pin."
  | Some set ->
    (* There is at most one existing 0-pin port in the chain.
       Every other port has to be marked as seen. *)
    PortSet.fold
      (fun element (size_acc, strength_acc)  ->
         let size, strength = eval_bridge ~size:1 0 element map set in
         if size_acc > size then (size_acc, strength_acc)
         else if size_acc = size then (size_acc, max strength strength_acc)
         else (size, strength))
      set (0, 0)


(* INPUT *)

let register n port map =
  let set' =
    match IntMap.find_opt n map with
    | None -> PortSet.singleton port
    | Some set -> PortSet.add port set
  in
  IntMap.add n set' map

let parse_ports line map =
  let v1, v2 = Scanf.sscanf line "%d/%d" (fun v1 v2 -> v1, v2 ) in
  let port = { v1; v2 } in
  register v1 port map |>
  register v2 port

let () =
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_all_lines ~start:IntMap.empty parse_ports)
    ~aoc_solver:start_bridge
    ~aoc_printer:(fun result -> string_of_int (snd result))
