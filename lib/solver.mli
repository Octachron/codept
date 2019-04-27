(** Basic solver *)

type i = { input: Unit.s; code: M2l.t With_deps.t }
(** In-processing state for unit files *)


(** Failure handling: detection of
    cycle, unresolvable dependencies and internal errors.
*)
module Failure :
  sig
    type status =
      | Cycle of Namespaced.t Loc.ext
      | Extern of Namespaced.t
      | Depend_on of Namespaced.t
      | Internal_error

    module Set: Set.S with type elt = i

    module Map : sig
      include Map.S with type key = status
      val find: key -> Set.t t -> Set.t
    end

    type alias_resolver = Summary.t -> Paths.S.t -> Namespaced.t

    val analyze:
      alias_resolver -> i list ->
      (i * status option ref) Namespaced.Map.t * Set.t Map.t

    val pp_circular :
      alias_resolver ->
      (i * 'a) Namespaced.Map.t ->
      Namespaced.t -> bool -> Format.formatter -> Namespaced.t -> unit
    val pp_cat :
      alias_resolver ->
      (i * _) Namespaced.Map.t ->
      Format.formatter -> status * Set.t -> unit
    val pp :
      alias_resolver ->
      (i * _) Namespaced.Map.t ->
      Format.formatter -> Set.t Map.t -> unit
    val pp_cycle : alias_resolver ->
      Format.formatter -> i list -> unit
  end

(** Solver error when trying to resolve dependencies *)
val fault: (Failure.alias_resolver * i list) Fault.info

(** Create a solver using the environment module [Envt] for
    name resolution and dependendy tracking and
    the parameter module [Param] *)
module Make(Envt:Outliner.envt)(Param : Outliner.param):
  sig

    type state = { resolved: Unit.r Paths.P.map;
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


      (** Resolve current aliases *)
      val alias_resolver: state -> Failure.alias_resolver

      (** Add approximation to make cycle resolvable, possibly adding spurious
          dependencies. Drop intermediary units that are deemed non-resolvable *)
      val approx_and_try_harder: state -> state

      (** Compare if two states would lead to the same result for the solver
          (weak equality?). *)
      val eq: state -> state -> bool

      (** Solve **)
       val solve: Envt.t -> Unit.s list Unit.pair -> Unit.r list Unit.pair

    end

(** Alternative solver *)
module Directed(Envt:Outliner.envt)(Param : Outliner.param):
sig
  type state

  (** Compare if two states would lead to the same result for the solver
      (weak equality?). *)
  val eq: state -> state -> bool

  val wip: state -> i list
  val end_result: state -> Envt.t * Unit.r list

  type gen = Namespaced.t -> Unit.s option Unit.pair

  (** Resolve current aliases *)
  val alias_resolver: state -> Failure.alias_resolver


  type entry = Read.kind * string * Namespaced.t
  type loader = entry -> Unit.s

  (** Generate unit files when needed from a
      loading function and a list of files *)
  val generator: loader -> entry list -> gen


  val start: loader -> entry list -> Envt.t -> Namespaced.t list ->
    state

  val eval: state -> i -> (state,state) result

  val solve_once: state -> (Envt.t * Unit.r list, state) result
  val approx_and_try_harder: state -> state
  val solve: loader -> entry list -> Envt.t -> Namespaced.t list
    -> Envt.t * Unit.r list

end
