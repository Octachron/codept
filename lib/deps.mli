
(** Edge type for qualifying dependencies *)
module Edge: sig
  type t =
    | Normal (**standard dependency *)
    | Epsilon (** immediate dependency *)
  val max: t -> t -> t
  val min: t -> t -> t
  val sch: t Schematic.t
end

type dep ={ path: Paths.S.t; edge:Edge.t; pkg:Paths.Pkg.t; aliases: Paths.S.set }
type t
val empty: t
val sch: t Schematic.t


(** Add a new path to a dependency map or
    promote the type of an existing path to {!Edge.Epsilon} *)
val update:
  path:Paths.S.t -> ?aliases:Paths.S.set -> edge:Edge.t -> Paths.P.t -> t -> t
val make:
  path:Paths.S.t -> ?aliases:Paths.S.set -> edge:Edge.t -> Paths.P.t -> t

val merge: t -> t -> t
val (+) : t -> t -> t

val pp: Format.formatter -> t -> unit

val find: Paths.S.t -> t -> dep option
val fold: (dep -> 'acc -> 'acc) -> t -> 'acc -> 'acc

val of_list: dep list -> t

val pkgs: t -> Paths.P.t list
val paths: t -> Paths.S.t list
val all: t -> dep list
val pkg_set: t -> Paths.P.set
