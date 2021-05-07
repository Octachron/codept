
(** Edge type for qualifying dependencies *)
module Edge: sig
  type t =
    | Normal (**standard dependency *)
    | Epsilon (** immediate dependency *)
  val max: t -> t -> t
  val min: t -> t -> t
  val sch: t Schematic.t
  val pp: t Pp.t
end

type dep ={ path: Namespaced.t; edge:Edge.t; pkg:Pkg.t; aliases: Namespaced.set }
type t
val empty: t
val sch: t Schematic.t


(** Add a new path to a dependency map or
    promote the type of an existing path to {!Edge.Epsilon} *)
val update:
  path:Namespaced.t -> ?aliases:Namespaced.set -> edge:Edge.t -> Pkg.t -> t -> t
val make:
  path:Namespaced.t -> ?aliases:Namespaced.set -> edge:Edge.t -> Pkg.t -> t

val merge: t -> t -> t
val (+) : t -> t -> t

val pp: Format.formatter -> t -> unit

val find: Namespaced.t -> t -> dep option
val fold: (dep -> 'acc -> 'acc) -> t -> 'acc -> 'acc

val of_list: dep list -> t

val pkgs: t -> Pkg.t list
val paths: t -> Namespaced.t list
val all: t -> dep list
val pkg_set: t -> Pkg.set
