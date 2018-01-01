
type coordinate =
  { x: int;
    y: int;
    z: int; }

type particle =
  { position: coordinate;
    velocity: coordinate;
    acceleration: coordinate; }

module PositionMap = Map.Make(struct
    type t = coordinate
    let compare = compare
  end)

let tick_particle { position; velocity; acceleration } =
  let velocity' =
    { x = velocity.x + acceleration.x;
      y = velocity.y + acceleration.y;
      z = velocity.z + acceleration.z; }
  in
  let position' =
    { x = position.x + velocity'.x;
      y = position.y + velocity'.y;
      z = position.z + velocity'.z }
  in
  { position = position';
    velocity = velocity';
    acceleration; }

let make_position_map particles =
  List.fold_left
    (fun position_map particle ->
       let particle' = tick_particle particle in
       match PositionMap.find_opt particle'.position position_map with
       | None ->
         PositionMap.add particle'.position [ particle' ] position_map
       | Some l ->
         PositionMap.add particle'.position (particle' :: l) position_map)
    PositionMap.empty
    particles

let remove_collision position_map =
  PositionMap.fold
    (fun _ particles acc ->
       match particles with
       | [] -> assert false     (* There is at least one particle *)
       | [ particle ] -> particle :: acc
       | _ -> acc)
    position_map
    []

let tick particles =
  let position_map = make_position_map particles in
  remove_collision position_map

let closest_to_origin particles =
  let count_before_stop = 100 in
  let rec loop ~count particles =
    if count > 0 then
      let particles = tick particles in
      loop ~count:(pred count) particles
    else
      List.length particles
  in
  loop ~count:count_before_stop particles


(* INPUT *)

let parse_line line =
  Scanf.sscanf line "p=<%d,%d,%d>, v=<%d,%d,%d>, a=<%d,%d,%d>"
    (fun px py pz vx vy vz ax ay az ->
       { position = { x = px; y = py; z = pz };
         velocity = { x = vx; y = vy; z = vz };
         acceleration = { x = ax; y = ay; z = az } })

let () =
  let parse_particle line acc = parse_line line :: acc in
  Aoc_solver.solve
    ~aoc_parser:(Aoc_solver.parser_all_lines_ordered ~start:[] parse_particle)
    ~aoc_solver:closest_to_origin
    ~aoc_printer:string_of_int
