(** Basic solver *)

(** Create a solver using the environment module [Envt] for
    name resolution and dependendy trackinf and
    the parameter module [Param] *)
module Make (Envt:Interpreter.envt_with_deps)(Param : Interpreter.param):
  sig
      exception Cycle of Envt.t * Unit.t list

      val eval :
        ?learn:bool ->
        Unit.t list * Envt.t * Unit.t list ->
        Unit.t -> Unit.t list * Envt.t * Unit.t list
      (** [eval ~learn (resolved, envt, unresolved) unit]
          try to compute the signature of unit, and if successful
          add the unit to the resolved list. Otherwise, the unit
          is added to the unresolved list.
          The learn parameter determines, if the environment is
          updated when the unit is fully resolved
      *)

      val resolve_dependencies :
        ?learn:bool -> Envt.t -> Unit.t list -> Envt.t * Unit.t list

      val resolve_split_dependencies :
        Envt.t -> Unit.t list Unit.pair -> Unit.t list Unit.pair
    end

(** Failure handling: detection of
    cycle, unresolvable dependencies and internal errors.
*)
module Failure :
  sig
    type status =
      | Cycle of string
      | Extern of string
      | Depend_on of string
      | Internal_error
    val analysis : Unit.t list -> (Unit.t * status option ref) Name.map
    module Map : sig
      include Map.S with type key = status
      val find: key -> Unit.Set.t t -> Unit.Set.t
    end

    val categorize :
      (Unit.t * status option ref) Name.map -> Unit.Set.t Map.t

    val kernel :
      (Unit.t * 'a) Name.map -> Unit.Set.t -> Unit.t -> Unit.Set.t

    val normalize :
      (Unit.t * 'a) Name.map -> Unit.Set.t Map.t -> Unit.Set.t Map.t

    val pp_circular :
      (Unit.t * 'a) Name.map ->
      string -> bool -> Format.formatter -> string -> unit
    val pp_cat :
      (Unit.t * 'a) Name.map ->
      Format.formatter -> status * Unit.Set.t -> unit
    val pp :
      (Unit.t * 'a) Name.map ->
      Format.formatter -> Unit.Set.t Map.t -> unit
    val pp_cycle : Format.formatter -> Unit.t list -> unit
  end
