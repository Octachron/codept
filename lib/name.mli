(** Basic function and modules over string as name *)

type t = string
type name = t
val pp: Format.formatter -> t -> unit

module Set: sig
  include Set.S with type elt = t
  val pp: Format.formatter -> t -> unit
end
type set = Set.t

module Map: sig
  include Map.S with type key = t
  val find_opt: key -> 'a t -> 'a option
  val union': 'a t -> 'a t -> 'a t
  val update: key -> ('a -> 'a) -> 'a t -> 'a t
end

type 'a map = 'a Map.t
