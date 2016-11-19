(** Monotonic interpreter for m2l ast *)

(** Input type *)
module type envt = sig
  type t
  val find: transparent:bool -> ?alias:bool ->
    Module.level -> Paths.Simple.t -> t -> Module.t
  val (>>) : t -> Module.signature -> t
  val add_module: t -> Module.t -> t
end

(** Interpreter parameter *)
module type param =
  sig
    val transparent_extension_nodes : bool
    val transparent_aliases : bool
  end

(** Create an interpreter adapted for the environment type *)
module Make :
  functor (Envt : envt) (Param : param) ->
    sig
      val m2l : Envt.t -> M2l.t -> (Envt.t * Module.Sig.t, M2l.t) result
    end
