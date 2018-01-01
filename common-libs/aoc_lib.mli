
(** This module contains some modules or extensions that is shared across
    all the repository. *)

(** This module provides extensions for the string type. *)
module ExtString: sig
  (** [get_opt input index] returns the character at the [index] of the
      [input]. If the index is out of bound, [None] is returned. *)
  val get_opt: string -> int -> char option

  (** [foldi fn a input] is (... (fn (fn a input.[0]) input.[1]) ...) until
      the last character of the given [input]. *)
  val foldi: (int -> 'a -> char -> 'a) -> 'a -> string -> 'a
end



module StringSet: Set.S with type elt := string
module IntSet: Set.S with type elt := int

module StringMap: Map.S with type key := string
module IntMap: Map.S with type key := int
