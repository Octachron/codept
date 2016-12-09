(** Basic solver *)

type i = { input: Unit.s; code: M2l.t; deps: Paths.P.set }

(** Create a solver using the environment module [Envt] for
    name resolution and dependendy trackinf and
    the parameter module [Param] *)
module Make (Envt:Interpreter.envt_with_deps)(Param : Interpreter.param):
  sig
      exception Cycle of Envt.t * i list

      val eval :
        ?learn:bool ->
        Unit.r list * Envt.t * i list ->
        i -> Unit.r list * Envt.t * i list
      (** [eval ~learn (resolved, envt, unresolved) unit]
          try to compute the signature of unit, and if successful
          add the unit to the resolved list. Otherwise, the unit
          is added to the unresolved list.
          The learn parameter determines, if the environment is
          updated when the unit is fully resolved
      *)

      val resolve_dependencies :
        ?learn:bool -> Envt.t -> Unit.s list -> Envt.t * Unit.r list

      val resolve_split_dependencies :
        Envt.t -> Unit.s list Unit.pair -> Unit.r list Unit.pair
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
    val analysis : i list -> (i * status option ref) Name.map

    module Set: Set.S with type elt = i

    module Map : sig
      include Map.S with type key = status
      val find: key -> Set.t t -> Set.t
    end

    val categorize :
      (i * status option ref) Name.map -> Set.t Map.t

    val kernel :
      (i * _) Name.map -> Set.t -> i -> Set.t

    val normalize :
      (i * _) Name.map -> Set.t Map.t -> Set.t Map.t

    val pp_circular :
      (i * 'a) Name.map ->
      string -> bool -> Format.formatter -> string -> unit
    val pp_cat :
      (i * _) Name.map ->
      Format.formatter -> status * Set.t -> unit
    val pp :
      (i * _) Name.map ->
      Format.formatter -> Set.t Map.t -> unit
    val pp_cycle : Format.formatter -> i list -> unit
  end
