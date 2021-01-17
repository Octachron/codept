(** Location information within a file *)

(** A location within a file *)
type t =
  | Nowhere
  | Simple of { line:int; start:int; stop:int }
  | Multiline of { start: int * int; stop: int * int }

(** A data structure to add location data *)
type 'a ext = { loc:t; data:'a }

val pp: Format.formatter -> t -> unit
val nowhere: 'a -> 'a ext
val create: t -> 'a -> 'a ext
val expand: t -> ((int*int) * (int*int)) option
val compress: t -> t
val merge: t -> t -> t
val keep_one: t -> t -> t
val fmap: ('a -> 'b) -> 'a ext -> 'b ext
val list: 'a ext list -> t

module Sch: sig
  val t: t Schematic.t
  val ext: 'a Schematic.t -> 'a ext Schematic.t
end
