(** Topological sort functions *)

(** [full_topopological_sort deps list] sorts [list] in a order compatible
    with the partial order induced by the dependency function [deps]
    if such an order exists (i.e. the dependency graph is acyclic. *)
val full_topological_sort:
  (Paths.Pkg.t -> Paths.Pkg.set) -> Paths.Pkg.t list -> Paths.Pkg.t list option

(** [order units] inspects an topologically ordered list [units] and
    remember the position of each modules *)
val remember_order: Unit.r list -> int Name.map

(** [topos_compare] uses a memoized order function to compare
    known modules *)
val topos_compare: int Name.map -> string -> string -> int

(** [toposort order proj l] sort the elements [elt] of [l] according to the
    order of their projection [proj elt] *)
val toposort: int Name.map -> ('a -> Name.t) -> 'a list -> 'a list
