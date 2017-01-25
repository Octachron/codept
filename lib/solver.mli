(** Basic solver *)

type i = { input: Unit.s; code: M2l.t; deps: Paths.P.set }
(** In-processing state for unit files *)


(** Failure handling: detection of
    cycle, unresolvable dependencies and internal errors.
*)
module Failure :
  sig
    type status =
      | Cycle of Name.t Loc.ext
      | Extern of Name.t
      | Depend_on of Name.t
      | Internal_error

    module Set: Set.S with type elt = i

    module Map : sig
      include Map.S with type key = status
      val find: key -> Set.t t -> Set.t
    end

    val analyze:
      i list -> (i * status option ref) Name.map * Set.t Map.t

    val pp_circular :
      (i * 'a) Name.map ->
      string -> bool -> Format.formatter -> Name.t Loc.ext -> unit
    val pp_cat :
      (i * _) Name.map ->
      Format.formatter -> status * Set.t -> unit
    val pp :
      (i * _) Name.map ->
      Format.formatter -> Set.t Map.t -> unit
    val pp_cycle : Format.formatter -> i list -> unit
  end

(** Create a solver using the environment module [Envt] for
    name resolution and dependendy tracking and
    the parameter module [Param] *)
module Make(Envt:Interpreter.envt_with_deps)(Param : Interpreter.param):
  sig

    type state = { resolved: Unit.r list;
                   env: Envt.t;
                   pending: i list;
                   postponed: Unit.s list
                 }

    val start: Envt.t -> Unit.s list -> state

      val eval :
        ?learn:bool -> state -> i -> state
      (** [eval ~learn {resolved; envt; rest} unit]
          try to compute the signature of unit, and if successful
          add the unit to the resolved list. Otherwise, the unit
          is added to the unresolved list.
          The learn parameter determines, if the environment is
          updated when the unit is fully resolved
      *)

      val resolve_dependencies :
        ?learn:bool -> state -> (Envt.t * Unit.r list, state) result

      (** Resolve mli unit first, then use the new environment to resolve
          ml units *)
      val resolve_split_dependencies :
        Envt.t -> Unit.s list Unit.pair ->
        (Unit.r list Unit.pair,
         [> `Mli of state
         | `Ml of (Unit.r list * state)]
        ) result

      (** Add approximation to make cycle resolvable, possibly adding spurious
          dependencies. Drop intermediary units that are deemed non-resolvable *)
      val approx_and_try_harder: state -> state
    end

(** Alternative solver *)
module Directed(Envt:Interpreter.envt_with_deps)(Param : Interpreter.param):
sig
  type state
  val wip: state -> i list
  val end_result: state -> Envt.t * Unit.r list

  type gen = Name.t -> Unit.s option Unit.pair

  val start: gen -> Envt.t -> Name.t list ->
    state

  val eval: state -> i -> (state,state) result

  val solve: state -> (Envt.t * Unit.r list, state) result
  val approx_and_try_harder: state -> state

end
