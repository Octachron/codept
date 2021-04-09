(** Environment type for name resolution *)
module type envt = sig
  type t
  val eq: t -> t -> bool
  val find:
    Uloc.t -> ?edge:Deps.Edge.t -> Module.level -> Paths.Simple.t
    -> t -> Transforms.answer Transforms.query_result

  val find_within:
    Module.signature ->
    Uloc.t -> ?edge:Deps.Edge.t -> Module.level -> Paths.Simple.t
    -> t -> Transforms.answer Transforms.query_result


  val extend : t -> Summary.t -> t

  val is_exterior: Paths.Simple.t -> t -> bool
  val resolve_alias: Paths.Simple.t -> t -> Namespaced.t option
  val expand_path: Paths.Simple.t -> t -> Paths.Simple.t

  val add_unit: t -> ?namespace:Paths.S.t -> Name.t -> Module.t -> t
  val add_namespace: t -> Namespaced.t -> t

  val pp: Format.formatter -> t -> unit
end

(** Parameter for outliner *)
module type param =
sig
  val policy: Fault.Policy.t
  val epsilon_dependencies: bool
  val transparent_extension_nodes : bool
  val transparent_aliases : bool
end

type 'a param = Transforms.param -> 'a
type 'a implicit = 'a

(** Outliner type *)
module type generic_outliner =
sig
  type envt
  type on_going
  type final
  type 'a with_param
  val initial: M2l.t -> on_going
  val next:
    pkg:Paths.P.t -> (envt -> on_going
    -> (Module.Sig.t * final, on_going) result) with_param

  val block: on_going -> (Summary.t * Paths.S.t) Loc.ext option

  val recursive_patching: on_going -> Summary.t -> on_going

  val pp: on_going Pp.t

end


module type outliner = generic_outliner with
  type final := Deps.t and type 'a with_param := 'a implicit
