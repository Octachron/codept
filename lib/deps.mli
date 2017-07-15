
(** Edge type for qualifying dependencies *)
module Edge: sig
  type t =
    | Normal (**standard dependency *)
    | Epsilon (** immediate dependency *)
  val max: t -> t -> t
  val min: t -> t -> t
  val sexp: (t, Sexp.one_and_many) Sexp.impl
end

type t = (Edge.t * Paths.S.set) Paths.P.map

val empty: t

(** Add a new path to a dependency map or
    promote the type of an existing path to {!Edge.Epsilon} *)
val update: Paths.P.t -> Edge.t -> Paths.S.set -> t -> t

val merge: t -> t -> t
val (+) : t -> t -> t

val pp: Format.formatter -> t -> unit

val of_list: (Paths.P.t * (Edge.t * Paths.S.set) ) list -> t

(** Forget edge type and go back to a simpler data structure *)
module Forget: sig
  val to_set: t -> Paths.P.set
  val to_list: t -> (Paths.P.t * Paths.S.set) list
end
