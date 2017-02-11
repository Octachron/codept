
type i = { input: Unit.s; code: M2l.t; deps: Deps.t }
let make input = { input; code = input.code; deps = Deps.empty }

module Failure = struct
  module Set = Set.Make(struct type t = i let compare = compare end)

  type status =
    | Cycle of Name.t Loc.ext
    | Extern of Name.t
    | Depend_on of Name.t
    | Internal_error


  let track_status (sources: i list) =
    let m = List.fold_left (fun m u -> Name.Map.add u.input.name (u, ref None) m )
        Name.Map.empty sources in
    let s = Name.Set.of_list @@ List.map (fun (x:i) -> x.input.name) @@ sources
    in
    let rec track s map ((u:i),r) =
      let s = Name.Set.remove u.input.name s in
      let update r = s, Name.Map.add u.input.name (u,r) map in
      match M2l.Block.m2l u.code with
      | None -> update (ref @@ Some Internal_error)
      | Some { data = (_y,path); loc } ->
        let name' = List.hd path
        (* FIXME: this can be the wrong name in presence of module alias *) in
        if not (Name.Map.mem name' map) then
         update (ref @@ Some (Extern name') )
        else
          let u', r' = Name.Map.find name' map in
          match !r' with
          | None ->
            if r' != r then begin
              let map = Name.Map.add u'.input.name (u',r) map in
              track s map (u',r)
            end
            else begin
              r:= Some (Cycle { data = u'.input.name; loc});
              s, map
            end
          | Some (Depend_on name| Cycle {data=name;_}) ->
            (r := Some (Depend_on name); s, map)
          | Some (Extern _ | Internal_error ) ->
            (r := Some (Depend_on u'.input.name); s, map)


    in
    let track_first s map  =
      let name = Name.Set.choose s in
      Name.Map.find name map |> track s map in
    let rec analyze map s =
      if Name.Set.cardinal s = 0 then
        map
      else
        let s, map = track_first s map in
        analyze map s in
    analyze m s


  module Map = struct
    include Map.Make(struct
        type t = status let compare = compare end)
    let find name m = try find name m with Not_found -> Set.empty
    let add_elt status unit m = add status (Set.add unit @@ find status m) m
    let add_set status set m =
      let union = Set.union set @@ find status m in
      add status union m

  end

  let categorize m = Name.Map.fold (fun _name (unit, status) ->
      Map.add_elt Option.(!status >< Internal_error) unit
    ) m Map.empty

  let rec kernel map cycle start =
    if Set.mem start cycle then cycle
    else
      let next_name = match M2l.Block.m2l start.code with
        | Some { data = _, x :: _  ; _ } -> x (* FIXME *)
        | _ -> assert false
      in
      let next = fst @@ Name.Map.find next_name map in
      kernel map (Set.add start cycle) next

  let rec ancestor_status name cmap =
    match !(snd @@ Name.Map.find name cmap) with
    | Some (Depend_on name) -> ancestor_status name cmap
    | Some status -> status
    | None -> Internal_error

  let normalize name_map map =
    Map.fold (function
        | Internal_error | Extern _ | Depend_on _ as st -> Map.add_set st
        | Cycle {data=name; _ } as st -> fun set map ->
          let u = fst @@ Name.Map.find name name_map in
          let k = kernel name_map Set.empty u in
          let map = Map.add st k map in
          let diff = Set.diff set k in
          if Set.cardinal diff > 0 then
            Map.add_set (Depend_on name) diff map
          else
            map
      ) map Map.empty

  let analyze sources =
    let map = track_status sources in
    map, categorize map |> normalize map


  let rec pp_circular map start first ppf name =
    Pp.fp ppf "%s" name;
    if name <> start || first then
      let u = fst @@ Name.Map.find name map in
      match M2l.Block.m2l u.code with
      | None | Some { data = _, []; _ } -> ()
      | Some { data = _y, next :: _ ; loc }  -> begin
          Pp.fp ppf " −(%a)⟶@ " Fault.loc (u.input.path, loc);
          pp_circular map start false ppf next
        end

  let pp_cat map ppf (st, units) =
    let paths units = List.map (fun u -> u.input.path) @@ Set.elements units in
    let path_pp = Paths.P.pp in
    match st with
    | Internal_error ->
      Pp.fp ppf "@[<hov 2> −Codept internal error for compilation units: {%a}@] "
        Pp.(list ~sep:(s ", @ ") @@ path_pp ) (paths units)

    | Extern name ->
      Pp.fp ppf "@[<hov 2> −Non-resolved external dependency.@;\
                 The following compilation units {%a} @ depend on \
                 the unknown module \"%s\" @]"
        Pp.(list ~sep:(s ", ") @@ path_pp ) (paths units)
        name
    | Depend_on name ->
      let u = fst @@ Name.Map.find name map in (* map ∋ name *)
      Pp.fp ppf "@[<hov 2> −Non-resolved internal dependency.@;\
                 The following compilation units {%a}@ depend on the compilation \
                 units \"%a\" that could not be resolved.@]"
        Pp.(list ~sep:(s ",@ ") @@ Paths.P.pp ) (paths units)
        path_pp u.input.path
    | Cycle name ->  Pp.fp ppf "@[<hov 4> −Circular dependencies: @ @[%a@]@]"
                        (pp_circular map name.data true) name.data

  let pp map ppf m =
    Pp.fp ppf "%a"
      Pp.(list ~sep:(s"\n@;") @@ pp_cat map ) (Map.bindings m)

  let pp_cycle ppf sources =
    let map, cmap = analyze sources in
    pp map ppf cmap

  let approx_cycle set (i:i) =
    let mock (unit: Unit.s) = Module.(md @@ mockup unit.name ~path:unit.path) in
    let add_set def =
      Set.fold
        (fun n acc -> Summary.see (mock n.input) acc) set def in
    let code =
      match i.code with
      | { data = M2l.Defs def; loc } :: q ->
        { Loc.data = M2l.Defs (add_set def); loc } :: q
      | code -> (Loc.nowhere @@ M2l.Defs (add_set Summary.empty)) :: code
    in
    { i with code }

    let approx_and_try_harder pending  =
    let cmap, map = analyze pending in
    let undo key x acc = match key with
      | Extern _ | Internal_error -> acc
      | Depend_on f ->
        begin
          match ancestor_status f cmap with
          | Cycle _ -> Set.union x acc
          | _ -> acc
        end
      | Cycle _ ->
        Set.elements x
        |> List.map (approx_cycle x)
        |> Set.of_list in
    Set.elements @@ Map.fold undo map Set.empty

end

let fault =
  Fault.{ path = ["solver"; "block" ];
    expl = "Solver fault: major errors during analysis.";
    log = (fun lvl -> log lvl
              "Solver failure@?@[@<2> @[<0>@;%a@]@]" Failure.pp_cycle
          )
  }


(** Common functions *)

  let eval_bounded policy eval (unit:Unit.s) =
    let unit' = Unit.{ unit with code = Approx_parser.to_upper_bound unit.code } in
    let r, r' = eval unit, eval unit' in
    let lower, upper = fst r, fst r' in
    let sign = match snd r, snd r' with
      | _ , Ok (_,sg)
      | Ok(_,sg) , Error _  ->  sg
      | Error _, Error _ -> Module.Sig.empty
        (* something bad happened but we are already parsing problematic
           input *) in
    let elts m = List.map fst @@ Paths.P.Map.bindings m in
    let set m = Paths.P.Set.of_list @@ elts m in
    if elts upper = elts lower then
      Fault.handle policy Standard_faults.concordant_approximation unit.path
    else
      Fault.handle policy Standard_faults.discordant_approximation
        unit.path
        (List.map Paths.P.module_name @@ elts lower)
        ( List.map Paths.P.module_name @@ Paths.P.Set.elements
          @@ Paths.P.Set.diff (set upper) (set lower));
    Unit.lift sign upper unit


let expand_epsilon resolved unit =
  let module M = Paths.P.Map in
  let open Unit in
  let add_epsilon_dep edge name e' deps =
    if e' = Deps.Edge.Epsilon then
      Deps.update name edge deps
    else
      deps in
  let expand_dep path edge deps =
    let deps = Deps.update path edge deps in
    match M.find_opt path resolved with
    | None -> deps
    | Some ancestor ->
      Paths.P.Map.fold (add_epsilon_dep edge) ancestor.dependencies deps in
  let deps = M.fold expand_dep unit.dependencies Deps.empty in
  { unit with dependencies = deps }

(* shortcut ε expansion when not needed *)
let expand_and_add expand  =
  if expand then
    fun resolved (unit:Unit.r) ->
      Paths.P.Map.add unit.path (expand_epsilon resolved unit) resolved
  else
    fun resolved (unit:Unit.r) ->
      Paths.P.Map.add unit.path unit resolved

module type s = functor (Envt : Interpreter.envt_with_deps)
  (Param : Interpreter.param) ->
  sig
    type state
    val start : Envt.t -> Unit.s list -> state
    val compute_more :
      Envt.t -> i -> Deps.t * (Envt.t * Module.Sig.t, M2l.t) result
    val eval : ?learn:bool -> state -> i -> state
    val eval_bounded : Envt.t -> Unit.s -> Unit.r
    val resolve_dependencies :
      ?learn:bool -> state -> (Envt.t * Unit.r list, state) result
    val resolve_split_dependencies :
      Envt.t ->
      Unit.s list Unit.pair ->
      (Unit.r list Unit.pair,
       [> `Ml of Unit.r list * state | `Mli of state ])
      result
    val approx_and_try_harder : state -> state
  end

module Make(Envt:Interpreter.envt_with_deps)(Param:Interpreter.param) = struct
  open Unit

  module Eval = Interpreter.Make(Envt)(Param)


  type state = { resolved: Unit.r Paths.P.map;
                 env: Envt.t;
                 pending: i list;
                 postponed: Unit.s list
               }

  let start env (units:Unit.s list) =
    List.fold_left (fun state (u:Unit.s) ->
        match u.precision with
        | Exact -> { state with pending = make u :: state.pending }
        | Approx ->
          let mock = Module.mockup u.name ~path:u.path in
          { state with postponed = u:: state.postponed;
                       env = Envt.add_unit state.env (Module.md mock)
          }
      )
      { postponed = [];
        env;
        pending = [];
        resolved = Paths.P.Map.empty } units

  let compute_more env (u:i) =
    let result = Eval.m2l u.input.path env u.code in
    let deps = Envt.deps env in
    Envt.reset_deps env;
    deps, result


  let expand_and_add = expand_and_add Param.epsilon_dependencies

  let eval ?(learn=true) state unit =
    match compute_more state.env unit with
    | deps, Ok (_,sg) ->
      let env =
        if learn then begin
          let input = unit.input in
          let md = Module.( create ~origin:(Unit input.path)) input.name sg in
          Envt.add_unit state.env (Module.M md)
        end
        else
          state.env
      in
      let deps = Deps.merge unit.deps deps in
      let unit = Unit.lift sg deps unit.input in
      { state with env; resolved = expand_and_add state.resolved unit }
    | deps, Error code ->
      let deps = Deps.merge unit.deps deps in
      let unit = { unit with deps; code } in
      { state with pending = unit :: state.pending }

  let eval_bounded core =
    eval_bounded Param.policy (fun unit -> compute_more core @@ make unit)

  let resolve_dependencies_main ?(learn=true) state =
    let rec resolve alert state =
      let n0 = List.length state.pending in
      let state =
        List.fold_left (eval ~learn) { state with pending = []}  state.pending in
      match List.length state.pending with
      | 0 -> Ok ( state.env, state.resolved )
      | n when n = n0 ->
        if alert then Error state
        else resolve true state
      | _ ->
        resolve false state in
    resolve false state

  let resolve_dependencies ?(learn=true) state =
    match resolve_dependencies_main ~learn state with
    | Ok (env, res) ->
      let units' = List.map (eval_bounded env) state.postponed in
      let res = List.fold_left expand_and_add res units' in
      let units = List.map snd @@ Paths.P.Map.bindings res in
      Ok (env, units )
    | Error _ as e -> e

  let resolve_split_dependencies env {ml; mli} =
    match resolve_dependencies ~learn:true @@ start env mli with
    | Ok  (env, mli) ->
      begin match resolve_dependencies ~learn:false @@ start env ml with
        | Ok(_, ml) -> Ok { ml; mli }
        | Error state -> Error( `Ml ( mli, state) )
      end
    | Error s -> Error (`Mli s)

  let approx_and_try_harder state  =
    { state with pending = Failure.approx_and_try_harder state.pending }

  let solve core (units: _ Unit.pair) =
    let rec solve_harder state =
      match resolve_dependencies ~learn:true state with
      | Ok (e,l) -> e, l
      | Error state ->
        Fault.handle Param.policy fault state.pending;
        solve_harder @@ approx_and_try_harder state in
    let env, mli = solve_harder @@ start core units.mli in
    let _, ml = solve_harder @@ start env units.ml in
    {Unit.ml;mli}

end



module Directed(Envt:Interpreter.envt_with_deps)(Param:Interpreter.param) = struct
  open Unit

  module Eval = Interpreter.Make(Envt)(Param)


  type gen = Name.t -> Unit.s option Unit.pair

  type state = {
    gen: gen;
    resolved: Unit.r Paths.P.map;
    learned: Name.set;
    not_ancestors: Name.set;
    pending: i list Name.map;
    env: Envt.t;
    postponed: Unit.s list;
    roots: Name.t list
  }

  let get name state =
    (* Invariant name ∈ state.pending, except when looking at a new seed *)
    List.hd @@ Name.Map.find name state.pending



  let remove name pending =
    match Name.Map.find name pending with
    | _ :: (b :: _ as q) -> Some b, Name.Map.add name q pending
    | [_]| [] -> None, Name.Map.remove name pending
    | exception Not_found ->
      (* happens when trying to remove itself due to
         a second round the same unit after encoutering an approximated module *)
      None, pending

  let add_pending i state =
    let l = Option.default [] @@ Name.Map.find_opt i.input.name state.pending in
    { state with
      pending = Name.Map.add i.input.name ( i :: l ) state.pending
    }

  let update_pending i state =
    let pending =
    match Name.Map.find i.input.name state.pending with
    | _ :: q -> Name.Map.add i.input.name (i::q) state.pending
    | [] -> Name.Map.add i.input.name [i] state.pending in
    { state with pending }

  let compute state i =
    let result = Eval.m2l i.input.path state.env i.code in
    let deps =Deps.( i.deps + Envt.deps state.env) in
    Envt.reset_deps state.env;
    deps, result

  let add_unit (u:Unit.s) state =
    match u.precision with
    | Exact -> add_pending (make u) state
    | Approx ->
      let mockup = Module.(md @@ mockup ~path:u.path u.name) in
      { state with env = Envt.add_unit state.env mockup;
                   postponed = u :: state.postponed
      }

  let add_pair state pair =
    match pair with
    | { ml = Some ml; mli = Some mli } ->
      add_unit mli @@ add_unit ml state
    | { ml = Some u; mli = None}
    | { mli = Some u; ml = None } -> add_unit u state
    | { ml = None; mli = None } -> state


  let add_name state name =
    add_pair state @@ state.gen name

  let expand_and_add = expand_and_add Param.epsilon_dependencies



  let rec eval_depth state i =
    let name, path = i.input.name, i.input.path in
    let not_ancestors = Name.Set.add name state.not_ancestors in
    let state = { state with not_ancestors } in
    match compute state i with
    | deps, Ok(_,sg) ->
      let more, pending = remove name state.pending in
      let state = { state with pending } in
      let state =
        if Name.Set.mem name state.learned then
          state
        else
          let md = Module.md @@ Module.(create ~origin:(Unit path)) name sg in
          { state with env = Envt.add_unit state.env md;
                       learned = Name.Set.add name state.learned
          } in
      let state =
        { state with
          resolved = expand_and_add state.resolved @@ Unit.lift sg deps i.input;
          not_ancestors = Name.Set.remove name state.not_ancestors
        } in
      Option.(more >>| eval_depth state >< Ok state )
    | deps, Error m2l ->
      (* first, we update the work-in-progress map *)
      let state = update_pending { i with deps; code = m2l } state in
      (* we check the first missing module *)
      match M2l.Block.m2l m2l with
      | None ->
        assert false (* we failed to eval the m2l code due to something *)
      | Some { Loc.data = y, path; _ } ->
        let env' = Envt.( state.env >> y ) in
        let first_parent =
        match Envt.resolve_alias path env' with
        | None -> List.hd path | Some name -> name in
        (* are we cycling? *)
        if Name.Set.mem first_parent state.not_ancestors then
          Error state
        else
          let go_on state i' =
            match eval_depth state i' with
            | Ok state -> eval_depth state i
            | Error state -> Error state in
          (* do we have an unit corresponding to this unit name *)
          match get first_parent state with
          | u -> go_on state u
          | exception Not_found ->
            match state.gen first_parent with
            | {mli=None; ml = None} -> Error state
            | ({ mli = Some u; _ } | { ml = Some u; mli = None } as pair) ->
              let state = add_pair state pair in
              if u.precision = Exact then
                go_on state (make u)
              else
                go_on state i

  let rec eval state i=
    let open Mresult.Ok in
    (eval_depth state i)
    >>=  (fun state ->
        if Name.Map.cardinal state.pending = 0 then
          Ok state
        else
          state.pending |> Name.Map.choose |> snd |> List.hd |> eval state
      )

  let eval_post state =
    let compute (u:Unit.s) = compute state @@ make u in
    let rec eval_more status state =
      match state.postponed with
      | [] -> status, state
      | u :: postponed ->
        let state =
          { (add_pending (make u) state) with postponed } in
      match u.precision with
      | Approx ->
        let r = eval_bounded Param.policy compute u in
        status, { state with
          resolved = expand_and_add state.resolved r
        }
      | Exact ->
        let state = add_pending (make u) state in
        match eval state (make u) with
        | Ok s -> eval_more status s
        | Error s -> eval_more false s
    in
    eval_more true state

  let wip state =
    List.flatten @@ List.map snd @@ Name.Map.bindings state.pending

  let end_result state =
    state.env, List.map snd @@ Paths.P.Map.bindings state.resolved


  let generator load_file files =
    let (++) = Unit.adder List.cons in
    let add_g (k,x) g = g ++ (k.Read.kind, (k,x) ) in
    let add m ((_,x) as f) =
      let name =  Paths.P.( module_name @@ local x) in
      let g = Option.default {Unit.ml = []; mli=[]} @@ Name.Map.find_opt name m in
      Name.Map.add name (add_g f g) m in
    let m = List.fold_left add Name.Map.empty files in
    let convert k l =
      match l with
      | [] -> None
      | [a] -> Some a
      | a :: _  ->
        Fault.handle Param.policy Standard_faults.local_module_conflict k
        @@ List.map (fun x -> Paths.P.local @@ snd x) l;
        Some a in
    let convert_p (k, p) = k, Unit.unimap (convert k) p
    in
    let m = List.fold_left (fun acc (k,x) -> Name.Map.add k x acc) Name.Map.empty
      @@ List.map convert_p @@ Name.Map.bindings m in
    fun name ->
      Name.Map.find_opt name m
      |> Option.default {Unit.ml = None; mli = None}
      |> Unit.unimap (Option.fmap load_file)


  let start gen env roots =
    {
      gen;
      resolved = Paths.P.Map.empty ;
      learned = Name.Set.empty;
      not_ancestors= Name.Set.empty;
      pending= Name.Map.empty;
      env;
      postponed= [];
      roots
    }

  let solve_once state =
    let rec solve_for_roots state = function
      | [] ->
        let ok, state = eval_post state in
        if ok then
          Ok (end_result state)
        else
          Error state
      | a :: q ->
        let state = add_name state a in
        match get a state with
        | exception Not_found -> solve_for_roots state q
        | u ->
          match eval state u with
          | Error s -> Error s
          | Ok state -> solve_for_roots state q in

    solve_for_roots state state.roots

  let flip f x y = f y x

  let approx_and_try_harder state =
    state
    |> wip
    |> Failure.approx_and_try_harder
    |> List.fold_left (flip add_pending)
      { state with pending = Name.Map.empty; not_ancestors = Name.Set.empty }

  let solve gen core seeds =
    let rec solve_harder state =
      match solve_once state with
      | Ok (e,l) -> e, l
      | Error s ->
        Fault.handle Param.policy fault (wip s);
        solve_harder @@ approx_and_try_harder s in
    solve_harder @@ start gen core seeds


end
