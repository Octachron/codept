(** Basic solver *)

(** Create a solver using the environment module [Envt] for
    name resolution and dependendy trackinf and
    the parameter module [Param] *)
module Make (Envt:Interpreter.envt_with_deps)(Param : Interpreter.param):
  sig
      exception Cycle of Envt.t * Unit.unit list

      val eval :
        ?learn:bool ->
        Unit.unit list * Envt.t * Unit.unit list ->
        Unit.unit -> Unit.unit list * Envt.t * Unit.unit list
      (** [eval ~learn (resolved, envt, unresolved) unit]
          try to compute the signature of unit, and if successful
          add the unit to the resolved list. Otherwise, the unit
          is added to the unresolved list.
          The learn parameter determines, if the environment is
          updated when the unit is fully resolved
      *)

      val resolve_dependencies :
        ?learn:bool -> Envt.t -> Unit.unit list -> Envt.t * Unit.unit list

      val resolve_split_dependencies :
        Envt.t -> Unit.unit list Unit.pair -> Unit.unit list Unit.pair
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
    val analysis : Unit.unit list -> (Unit.unit * status option ref) Name.map
    module Map : sig
      include Map.S with type key = status
      val find: key -> Unit.Set.t t -> Unit.Set.t
    end

    val categorize :
      (Unit.unit * status option ref) Name.map -> Unit.Set.t Map.t

    val kernel :
      (Unit.unit * 'a) Name.map -> Unit.Set.t -> Unit.unit -> Unit.Set.t

    val normalize :
      (Unit.unit * 'a) Name.map -> Unit.Set.t Map.t -> Unit.Set.t Map.t

    val pp_circular :
      (Unit.unit * 'a) Name.map ->
      string -> bool -> Format.formatter -> string -> unit
    val pp_cat :
      (Unit.unit * 'a) Name.map ->
      Format.formatter -> status * Unit.Set.t -> unit
    val pp :
      (Unit.unit * 'a) Name.map ->
      Format.formatter -> Unit.Set.t Map.t -> unit
    val pp_cycle : Format.formatter -> Unit.unit list -> unit
  end
