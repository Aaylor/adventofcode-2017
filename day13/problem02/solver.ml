
type layer =
  { depth: int;
    current_position: int;
    next: int }

(* Packets *)

exception Has_been_caught

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
  let rec loop idx_packet =
    (* First step: move the packet.
       If the packet is above the last layer, it's over. *)
    let next_packet = succ idx_packet in
    if next_packet < Array.length layers then begin
      (* Second step: check if it's on a layer *)
      begin
        match layers.(next_packet) with
        | None -> ()
        | Some layer -> if layer.current_position = 0 then raise Has_been_caught
      end;
      (* Third step: move scanners *)
      move_scanners layers;
      (* Last step: restart the loop *)
      loop next_packet
    end
  in
  loop (-1)

let find_delay layers =
  (* This is totally bruteforcing...
     Maybe do better next time ? *)
  let rec aux_find_delay last_layer count =
    let new_layer = Array.copy last_layer in
    move_scanners new_layer;
    let new_layer_saved = Array.copy new_layer in
    try
      run_packet_scanner new_layer;
      count
    with Has_been_caught ->
      aux_find_delay new_layer_saved (succ count)
  in
  (* We must delay at least once, otherwise it will be caught
     by the first scanner. *)
  aux_find_delay layers 1

let eval_packet_scanner (max_layer, layers) =
  let layers_array = Array.make (max_layer + 1) None in
  List.iter
    (fun (layer, depth) ->
       layers_array.(layer) <- Some { depth; current_position = 0; next = 1 })
    layers;
  find_delay layers_array


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
