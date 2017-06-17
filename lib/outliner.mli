(** Monotonic outliner for m2l ast *)

(** Input fault type *)
type 'a query_result = { main:'a; msgs: (Fault.loc -> unit ) Fault.t list }
type answer =
  | M of Module.m
  | Namespace of { name:Name.t; modules:Module.dict }

(** Input type *)
module type envt = sig
  type t
  val is_exterior: Paths.Simple.t -> t -> bool
  val find: ?edge:Deps.Edge.t -> Module.level -> Paths.Simple.t -> t ->
    answer query_result
  val (>>) : t -> Summary.t -> t
  val resolve_alias: Paths.Simple.t -> t -> Namespaced.t option
  val add_unit: t -> ?namespace:Paths.S.t -> Module.t -> t
end

module type with_deps = sig
  type t
  val deps: t -> Deps.t
  val reset_deps: t -> unit
end

module type envt_with_deps = sig
  type t
  include envt with type t := t
  include with_deps with type t := t
end

(** Interpreter parameter *)
module type param =
sig
  val policy: Fault.Policy.t
  val epsilon_dependencies: bool
  val transparent_extension_nodes : bool
  val transparent_aliases : bool
end

(** resulting signature *)
module type s =
sig
  type envt
  val m2l : Paths.P.t -> envt -> M2l.t -> (envt * Module.Sig.t, M2l.t) result
end

(** Create an outliner adapted for the environment type *)
module Make :
  functor (Envt : envt) (Param : param) -> s with type envt := Envt.t
