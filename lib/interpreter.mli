(** Monotonic interpreter for m2l ast *)

(** Input type *)
module type envt = sig
  type t
  val find: transparent:bool -> ?alias:bool ->
    Module.level -> Paths.Simple.t -> t -> Module.t
  val (>>) : t -> Module.signature -> t
  val add_module: t -> Module.t -> t
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
    val transparent_extension_nodes : bool
    val transparent_aliases : bool
  end

(** resulting signature *)
module type s =
sig
  type envt
  val m2l : envt -> M2l.t -> (envt * Module.Sig.t, M2l.t) result
end

(** Create an interpreter adapted for the environment type *)
module Make :
  functor (Envt : envt) (Param : param) -> s with type envt := Envt.t
