(** Different environment implementations *)

(** Extended environment for composition *)
module type extended =
sig
  open Outliner
  include envt

  val top: t -> t
  (** Return to toplevel definitions *)

  val find_name : ?edge:Deps.Edge.t -> bool -> Module.level -> string -> t
    -> Module.t Outliner.query_result
(** [find_name is_root level name env] find if there is a module [name]
    at [level] in the environment [env]. The first argument indicates
    if we are looking for a toplevel module, this is useful for both
    dependency tracking and when using external dependency.
*)

  val restrict : t -> Module.signature -> t
(** [restrict env sign] results in the environment restricted to
    the identifiers visible from [sign]
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
  type t = { top: Module.definition; current: Module.signature }
  include extended_with_deps with type t := t
  val empty: t
  val start: Module.definition -> t
end

(** Extend environment with unknowable module handling *)
module Open_world :
  functor (Envt : extended_with_deps ) ->
  sig
    type t ={ core : Envt.t;
              world : Name.set;
              externs : Deps.t ref; }

    include extended_with_deps with type t := t
    val start: Envt.t -> Name.set -> t
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
  type t = { local : Base.t; local_units:Name.set; pkgs : source list; }

  val create : string list -> Name.set -> Base.t -> t
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
