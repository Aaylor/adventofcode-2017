
(** {2 Extensions} *)

module ExtString = struct
  let get_opt string index =
    try Some (String.get string index)
    with Invalid_argument _ -> None

  let foldi fn acc input =
    let rec aux acc index =
      match get_opt input index with
      | None ->
        acc
      | Some character ->
        let acc = fn index acc character in
        aux acc (succ index)
    in
    aux acc 0
end


(** {2 Common Map & Set modules} *)

module Int = struct
  type t = int
  let compare = compare
end

module StringSet = Set.Make(String)
module IntSet = Set.Make(Int)

module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)
