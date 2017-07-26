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
val fmap: ('a -> 'b) -> 'a ext -> 'b ext
val list: 'a ext list -> t

module Sexp: sig
  val t: (t,Sexp.one_and_many) Sexp.impl
  val ext:  ('a,'b) Sexp.impl -> ('a ext, Sexp.many) Sexp.impl
end

module Sch: sig
  val t: t Schematic.t
  val ext: Name.t -> 'a Schematic.t -> 'a ext Schematic.t
end
