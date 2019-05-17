(** Monotonic outliner for m2l ast *)


(** Input type *)
module type envt = sig
  type t
  val eq: t -> t -> bool
  val find:
    Fault.loc -> ?edge:Deps.Edge.t -> Module.level -> Paths.Simple.t
    -> t -> Transforms.answer Transforms.query_result

  val extend : t -> Summary.t -> t

  val is_exterior: Paths.Simple.t -> t -> bool
  val resolve_alias: Paths.Simple.t -> t -> Namespaced.t option
  val expand_path: Paths.Simple.t -> t -> Paths.Simple.t

  val add_unit: t -> ?namespace:Paths.S.t -> Module.t -> t
  val add_namespace: t -> Namespaced.t -> t

  val pp: Format.formatter -> t -> unit
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
  type on_going
  val initial: M2l.t -> on_going
  val next:
    pkg:Paths.P.t -> envt -> on_going
    -> (Module.Sig.t * Deps.t, on_going) result

  val block: on_going -> (Summary.t * Paths.S.t) Loc.ext option

  val recursive_patching: on_going -> Summary.t -> on_going

  val pp: on_going Pp.t

end

(** Create an outliner adapted for the environment type *)
module Make :
  functor (Envt : envt) (Param : param) -> s with type envt := Envt.t
