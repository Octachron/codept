(** Topological sort functions *)

(** [full_topopological_sort deps list] sorts [list] in a order compatible
    with the partial order induced by the dependency function [deps]
    if such an order exists (i.e. the dependency graph is acyclic. *)
val full_topological_sort:
 key:('a -> 'k) -> ('a -> 'a list) -> 'a list -> ('a list, 'a list) Mresult.t

(** [order units] inspects an topologically ordered list [units] and
    remember the position of each modules *)
val remember_order: Unit.r list -> int Namespaced.Map.t

(** [topos_compare] uses a memoized order function to compare
    known modules *)
val topos_compare: int Namespaced.Map.t -> Namespaced.t
  -> Namespaced.t -> int

(** [toposort order proj l] sort the elements [elt] of [l] according to the
    order of their projection [proj elt] *)
val toposort: int Namespaced.Map.t -> ('a -> Namespaced.t)
  -> 'a list -> 'a list
