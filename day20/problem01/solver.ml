
type coordinate =
  { x: int;
    y: int;
    z: int; }

type particle =
  { position: coordinate;
    velocity: coordinate;
    acceleration: coordinate; }

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

let manhattan_distance particle =
  abs particle.position.x + abs particle.position.y + abs particle.position.z

let tick particles =
  let rec aux_tick ~index ~min_value ~min_index ~mapped_particles particles =
    match particles with
    | [] ->
      min_index,
      List.rev mapped_particles
    | particle :: particles ->
      let particle' = tick_particle particle in
      let distance = manhattan_distance particle' in
      let min_value, min_index =
        match min_value with
        | None ->
          Some distance, index
        | Some distance' ->
          if distance < distance' then Some distance, index
          else Some distance', min_index
      in
      aux_tick
        ~index:(succ index) ~min_value ~min_index
        ~mapped_particles:(particle' :: mapped_particles)
        particles
  in
  aux_tick ~index:0 ~min_value:None ~min_index:0 ~mapped_particles:[] particles

let closest_to_origin particles =
  let count_before_stop = 200 in
  let rec loop ~min_index ~count particles =
    if count > 0 then
      let min_index', particles' = tick particles in
      let min_index, count =
        if min_index <> min_index'
        then min_index', count_before_stop
        else min_index, pred count
      in
      loop ~min_index ~count particles'
    else
      min_index
  in
  loop ~min_index:0 ~count:count_before_stop particles


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
