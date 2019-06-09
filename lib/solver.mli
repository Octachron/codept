(** Basic solver *)

type 'a i = { input: Unit.s; code: 'a }
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

    type 'a cycle
    type 'a cycles

    val to_list: 'a cycles -> (status * Unit.s list) list

    type alias_resolver = Summary.t -> Paths.S.t -> Namespaced.t
    type 'a blocker = 'a -> (Summary.t * Paths.S.t) Loc.ext option

    val analyze:
      'a blocker -> alias_resolver -> 'a i list ->
      ('a i * status option ref) Namespaced.Map.t * 'a cycles

    val pp_circular :
      'a blocker -> alias_resolver ->
      ('a i * 'a) Namespaced.Map.t ->
      Namespaced.t -> bool -> Format.formatter -> Namespaced.t -> unit
    val pp_cat :
      'a blocker -> alias_resolver ->
      ('a i * _) Namespaced.Map.t ->
      Format.formatter -> status * 'a cycle -> unit
    val pp :
      'a blocker -> alias_resolver ->
      ('a i * _) Namespaced.Map.t ->
      Format.formatter -> 'a cycles -> unit
    val pp_cycle : 'a blocker -> alias_resolver ->
      Format.formatter -> 'a i list -> unit
  end

(** Solver error when trying to resolve dependencies *)
type fault
val fault: (fault * Failure.alias_resolver) Fault.info

(** Create a solver using the environment module [Envt] for
    name resolution and dependendy tracking and
    the parameter module [Param] *)
module Make
    (Envt:Stage.envt)
    (Param : Stage.param)
    (Eval: Stage.outliner with type envt := Envt.t):
  sig

    type state = { resolved: Unit.r Paths.P.map;
                   env: Envt.t;
                   pending: Eval.on_going i list;
                   postponed: Unit.s list
                 }

    val start: Envt.t -> Unit.s list -> state

      val eval :
        ?learn:bool -> state -> Eval.on_going i -> state
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

      (** expose current blocker on an on_going element *)
      val blocker: Eval.on_going Failure.blocker

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
module Directed
    (Envt:Stage.envt)
    (Param : Stage.param)
    (Eval: Stage.outliner with type envt := Envt.t):
sig
  type state

  (** Compare if two states would lead to the same result for the solver
      (weak equality?). *)
  val eq: state -> state -> bool

  val wip: state -> Eval.on_going i list
  val end_result: state -> Envt.t * Unit.r list

  type gen = Namespaced.t -> Unit.s option Unit.pair

  (** Resolve current aliases *)
  val alias_resolver: state -> Failure.alias_resolver

  (** expose current blocker on an on_going element *)
  val blocker: Eval.on_going Failure.blocker



  type entry = Read.kind * string * Namespaced.t
  type loader = entry -> Unit.s

  (** Generate unit files when needed from a
      loading function and a list of files *)
  val generator: loader -> entry list -> gen


  val start: loader -> entry list -> Envt.t -> Namespaced.t list ->
    state

  val eval: state -> Eval.on_going i -> (state,state) result


  val solve_once: state -> (Envt.t * Unit.r list, state) result
  val approx_and_try_harder: state -> state
  val solve: loader -> entry list -> Envt.t -> Namespaced.t list
    -> Envt.t * Unit.r list

end
