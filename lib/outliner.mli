(** Monotonic outliner for m2l ast *)

(** Input fault type *)
type 'a query_result = { main:'a; deps: Deps.t; msgs: (Fault.loc -> unit ) Fault.t list }
type answer =
  | M of Module.m
  | Namespace of Module.namespace_content

(** Input type *)
module type envt = sig
  type t
  val eq: t -> t -> bool
  val find: ?edge:Deps.Edge.t -> Module.level -> Paths.Simple.t
    -> t
    -> answer query_result

  val (>>) : t -> Summary.t -> t

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

module With_deps: sig
  type 'a t
  val no_deps: 'a -> 'a t
  val deps: 'a t -> Deps.t
  val value: 'a t -> 'a
  val unpack: 'a t -> Deps.t * 'a
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val map: 'a t -> ('a -> 'b) -> 'b t
  val comm: ('a,'b) result t -> ('a t, 'b t) result
end

(** resulting signature *)
module type s =
sig
  type envt
  val m2l : Paths.P.t -> envt -> M2l.t -> (envt *  Module.Sig.t, M2l.t) result With_deps.t
end

(** Create an outliner adapted for the environment type *)
module Make :
  functor (Envt : envt) (Param : param) -> s with type envt := Envt.t
