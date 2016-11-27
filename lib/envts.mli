(** Different environment implementations *)

(** Extended environment for composition *)
module type extended =
sig
  open Interpreter
  include envt

  val find_name : bool -> Module.level -> string -> t -> Module.t
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
  include Interpreter.with_deps with type t := t
end

(** Basic environment *)
module Base :
sig
  include extended_with_deps with type t = Module.signature
  val empty: t
end

(** Extend environment with unknowable module handling *)
module Open_world :
  functor (Envt : extended_with_deps ) ->
  sig
    type t ={ core : Envt.t;
              world : Paths.Pkg.t Name.map;
              externs : Paths.Pkg.set ref; }

    include extended_with_deps with type t := t
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
      type t = { env : Envt.t; deps : Paths.Pkg.set ref; }
      include extended with type t := t
      val extend: Envt.t -> t
    end

module Trl :
sig
  type t = Tracing(Layered).t =
    { env : Layered.t; deps : Paths.Pkg.set ref; }
  include extended_with_deps with type t:= t
  val extend: Layered.t -> t
end


module Tr :
sig
  type t = Open_world(Trl).t =
    { core : Trl.t;
      world : Paths.Pkg.t Name.map;
      externs : Paths.Pkg.set ref; }

  include extended_with_deps with type t := t
  val start : Trl.t -> Paths.Pkg.t Name.map -> t
end

module Interpreters :
  sig
    module Sg :
      functor (Param : Interpreter.param) ->
        sig val m2l : Base.t -> M2l.t -> (Base.t * Base.t, M2l.t) result end
    module Tr :
      functor (Param : Interpreter.param) ->
        sig val m2l : Tr.t -> M2l.t -> (Tr.t * Base.t, M2l.t) result end
  end
