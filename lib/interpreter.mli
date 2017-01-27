(** Monotonic interpreter for m2l ast *)

(** Input fault type *)
type 'a query_result = { main:'a; msgs: (Fault.loc -> unit ) Fault.t list }

(** Input type *)
module type envt = sig
  type t
  val is_exterior: Paths.Simple.t -> t -> bool
  val find: Module.level -> Paths.Simple.t -> t ->
    Module.m query_result
  val (>>) : t -> Summary.t -> t
  val add_unit: t -> Module.t -> t
end

module type with_deps = sig
  type t
  val deps: t -> Paths.Pkg.set
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
  val transparent_extension_nodes : bool
  val transparent_aliases : bool
end

(** resulting signature *)
module type s =
sig
  type envt
  val m2l : Paths.P.t -> envt -> M2l.t -> (envt * Module.Sig.t, M2l.t) result
end

(** Create an interpreter adapted for the environment type *)
module Make :
  functor (Envt : envt) (Param : param) -> s with type envt := Envt.t
