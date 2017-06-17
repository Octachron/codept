(** Different environment implementations *)


type answer = Outliner.answer =
  | M of Module.m
  | Namespace of { name:Name.t; modules:Module.dict }

type context =
  | Signature of Module.signature
  | In_namespace of Module.dict

(** Extended environment for composition *)
module type extended =
sig
  open Outliner
  include envt

  val top: t -> t
  (** Return to toplevel definitions *)

  val find_name : ?edge:Deps.Edge.t -> root:bool -> Module.level
    -> Name.t -> t -> Module.t Outliner.query_result
(** [find_name is_root level name env] find if there is a module [name]
    at [level] in the environment [env]. The first argument indicates
    if we are looking for a toplevel module, this is useful for both
    dependency tracking and when using external dependency.
*)

  val restrict : t -> context -> t
(** [restrict env context] results in the environment restricted to
    the identifiers visible from [context]
*)

end

module type extended_with_deps =
sig
  type t
  include extended with type t:=t
  include Outliner.with_deps with type t := t
end

(** Basic environment *)
module Base :
sig
  type t = { top: Module.dict; current: context }
  include extended_with_deps with type t := t
  val empty: t
  val start: Module.dict -> t
end

(** Extend environment with unknowable module handling *)
module Open_world :
  functor (Envt : extended_with_deps ) ->
  sig
    type t ={ core : Envt.t;
              world : Name.Set.t; (** root namespace and modules *)
              externs : Deps.t ref; }

    include extended_with_deps with type t := t
    val start: Envt.t -> Name.Set.t -> t
end


(** Environment with external package handling *)
module Layered :
sig
  module Envt: extended with type t = Open_world(Base).t
  type source = {
    origin: Paths.Simple.t;
    mutable resolved: Envt.t;
    cmis: Paths.Pkg.t Name.map
  }
  type t = { local : Base.t;
             local_units:Name.Set.t;
             pkgs : source list; }

  val create : string list -> Name.Set.t -> Base.t -> t
  include extended with type t := t

end

(** Extend environment with dependencies tracking *)
module Tracing :
  functor (Envt : extended) ->
    sig
      type t = { env : Envt.t; deps : Deps.t ref; }
      include extended_with_deps with type t := t
      val extend: Envt.t -> t
    end

module Trl: module type of Tracing(Layered)
module Tr: module type of Open_world(Trl)
