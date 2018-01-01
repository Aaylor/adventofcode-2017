
type layer =
  { depth: int;
    current_position: int;
    next: int }

(* Packets *)

let move_scanners layers =
  (* Without some lazyness, I should only track layers' indexes instead of
     iterating on every elements.
     But lazyness is lazyness... *)
  Array.iteri
    (fun idx olayer ->
       match olayer with
       | None ->
         ()
       | Some layer ->
         let next_position = layer.current_position + layer.next in
         let layer' =
           let next =
             if next_position = layer.depth - 1 then -1
             else if next_position = 0 then 1
             else layer.next
           in
           { layer with
             current_position = next_position;
             next }
         in
         layers.(idx) <- Some layer')
    layers

let run_packet_scanner layers =
  let rec loop sum idx_packet =
    (* First step: move the packet.
       If the packet is above the last layer, it's over. *)
    let next_packet = succ idx_packet in
    if next_packet < Array.length layers then begin
      (* Second step: check if it's on a layer *)
      let sum' =
        match layers.(next_packet) with
        | None ->
          sum
        | Some layer ->
          if layer.current_position = 0
          then (next_packet * layer.depth) + sum
          else sum
      in
      (* Third step: move scanners *)
      move_scanners layers;
      (* Last step: restart the loop *)
      loop sum' next_packet
    end else
      sum
  in
  loop 0 (-1)

let eval_packet_scanner (max_layer, layers) =
  let layers_array = Array.make (max_layer + 1) None in
  List.iter
    (fun (layer, depth) ->
       layers_array.(layer) <- Some { depth; current_position = 0; next = 1 })
    layers;
  run_packet_scanner layers_array


(* INPUT *)

let extract_lines line (max_layer, acc) =
  let layer, depth =
    Scanf.sscanf line
      "%d: %d"
      (fun layer depth -> layer, depth)
  in
  (max max_layer layer), ((layer, depth) :: acc)

let () =
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_all_lines ~start:(0, []) extract_lines)
    ~aoc_solver:eval_packet_scanner
    ~aoc_printer:string_of_int
