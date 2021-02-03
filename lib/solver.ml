
module With_deps = With_deps

let debug fmt = Format.ifprintf Pp.err ("Debug:" ^^ fmt ^^"@.")

type 'a i = { input: Unit.s; code: 'a}
let make initial (input:Unit.s) =
  let open_namespace = List.map
      (fun name -> Loc.nowhere @@ M2l.Open (Ident [name])) input.path.namespace in
  { input; code = initial (open_namespace @ input.code) }

module Mp = Namespaced.Map module Sp = Namespaced.Set

module Failure = struct
  module UMap = Map.Make(struct type t = Unit.s let compare = compare end)

  type 'a cycle = 'a UMap.t
  type status =
    | Cycle of Namespaced.t Loc.ext
    | Extern of Namespaced.t
    | Depend_on of Namespaced.t
    | Internal_error

  type alias_resolver = Summary.t -> Paths.S.t -> Namespaced.t
  type block = (Summary.t * Paths.S.t) Loc.ext option
  type 'a blocker = 'a -> block

  let track_status block resolve_alias (sources: _ i list) =
    let m =
      List.fold_left (fun m u -> Mp.add u.input.path (u, ref None) m)
        Mp.empty sources in
    let s =
      Sp.of_list @@ List.map (fun (x: _ i) -> x.input.path) @@ sources
    in
    let rec track s map ((u:_ i),r) =
      let s = Sp.remove u.input.path s in
      let update r = s, Mp.add u.input.path (u,r) map in
      match block u.code with
      | None -> update (ref @@ Some Internal_error)
      | Some { Loc.data = (y,path); loc } ->
        let path' = resolve_alias y path in
        if not (Mp.mem path' map) then
         update (ref @@ Some (Extern path') )
        else
          let u', r' = Mp.find path' map in
          match !r' with
          | None ->
            if r' != r then begin
              let map = Mp.add u'.input.path (u',r) map in
              track s map (u',r)
            end
            else begin
              r:= Some (Cycle { data = u'.input.path; loc});
              s, map
            end
          | Some (Depend_on name| Cycle {data=name;_}) ->
            (r := Some (Depend_on name); s, map)
          | Some (Extern _ | Internal_error ) ->
            (r := Some (Depend_on u'.input.path); s, map)


    in
    let track_first s map  =
      let name = Sp.choose s in
      Mp.find name map |> track s map in
    let rec analyze map s =
      if Sp.cardinal s = 0 then
        map
      else
        let s, map = track_first s map in
        analyze map s in
    analyze m s


  module Map = struct
    include Map.Make(struct type t = status let compare = compare end)
    let find name m = try find name m with Not_found -> UMap.empty
    let add_elt status unit m =
      add status (UMap.add unit.input unit.code @@ find status m) m
    let add_set status set m =
      let union = UMap.union (fun _ _ y -> Some y) set @@ find status m in
      add status union m

  end
  type 'a cycles = 'a cycle Map.t

  let to_list x =
    x
    |> Map.bindings
    |> List.map (fun (k,x) -> k, List.map fst (UMap.bindings x))

  let categorize m = Mp.fold (fun _name (unit, status) ->
      Map.add_elt Option.(!status >< Internal_error) unit
    ) m Map.empty

  let rec kernel block resolver map cycle start =
    if UMap.mem start.input cycle then cycle
    else
      let next_name =
        match block start.code with
        | Some { Loc.data = y, path ; _ } ->
          resolver y path
        | _ -> assert false
      in
      let next = fst @@ Mp.find next_name map in
      kernel block resolver map (UMap.add start.input start.code cycle) next

  let rec ancestor_status name cmap =
    match !(snd @@ Mp.find name cmap) with
    | Some (Depend_on name) -> ancestor_status name cmap
    | Some status -> status
    | None -> Internal_error

  let normalize block resolver name_map map =
    Map.fold (function
        | Internal_error | Extern _ | Depend_on _ as st -> Map.add_set st
        | Cycle {data=name; _ } as st -> fun set map ->
          let u = fst @@ Mp.find name name_map in
          let k = kernel block resolver name_map UMap.empty u in
          let map = Map.add st k map in
          let diff =
            UMap.fold
              (fun key x m -> if UMap.mem key k then m else UMap.add key x m)
              set UMap.empty in
          if UMap.cardinal diff > 0 then
            Map.add_set (Depend_on name) diff map
          else
            map
      ) map Map.empty

  let analyze block resolver sources =
    let map = track_status block resolver sources in
    map, categorize map |> normalize block resolver map


  let rec pp_circular block resolver map start first ppf path =
    Pp.fp ppf "%a" Namespaced.pp path;
    if path <> start || first then
      let u = fst @@ Mp.find path map in
      match block u.code with
      | None -> ()
      | Some { Loc.data = y,path ; loc }  ->
        let next = resolver y path in
        begin
          Pp.fp ppf " −(%a)⟶@ " Fault.loc (u.input.src, loc);
          pp_circular block resolver map start false ppf next
        end

  let pp_cat block resolver map ppf (st, units) =
    let paths units =
      List.rev @@ UMap.fold (fun k _ l -> k.src :: l) units [] in
    let path_pp = Paths.P.pp in
    match st with
    | Internal_error ->
      Pp.fp ppf "@[<hov 2> −Codept internal error for compilation units: {%a}@] "
        Pp.(list ~sep:(s ", @ ") @@ path_pp ) (paths units)

    | Extern path ->
      Pp.fp ppf "@[<hov 2> −Non-resolved external dependency.@;\
                 The following compilation units {%a} @ depend on \
                 the unknown module \"%a\" @]"
        Pp.(list ~sep:(s ", ") @@ path_pp ) (paths units)
        Namespaced.pp path
    | Depend_on path ->
      let u = fst @@ Mp.find path map in (* map ∋ name *)
      Pp.fp ppf
        "@[<hov 2> −Non-resolved internal dependency.@;\
         The following compilation units {%a}@ depend on the \
         compilation units \"%a\" that could not be resolved.@]"
        Pp.(list ~sep:(s ",@ ") @@ Paths.P.pp ) (paths units)
        path_pp u.input.src
    | Cycle name ->
      Pp.fp ppf "@[<hov 4> −Circular dependencies: @ @[%a@]@]"
        (pp_circular block resolver map name.data true) name.data

  let pp block resolver map ppf m =
    Pp.fp ppf "%a"
      Pp.(list ~sep:(s"\n@;") @@ pp_cat block resolver map ) (Map.bindings m)

  let pp_cycle block resolver ppf sources =
    let map, cmap = analyze block resolver sources in
    pp block resolver map ppf cmap

  let approx_cycle recpatch set code =
    let mock (unit: Unit.s) =
      unit.path.name, Module.(md @@ mockup unit.path.name ~path:unit.src) in
    let cycle_summary =
      UMap.fold
        (fun k _ acc -> Summary.see (mock k) acc) set Summary.empty in
    recpatch code cycle_summary

    let approx_and_try_harder recpatch block resolver pending  =
    let cmap, map = analyze block resolver pending in
    let undo key x acc = match key with
      | Extern _ | Internal_error -> acc
      | Depend_on f ->
        begin
          match ancestor_status f cmap with
          | Cycle _ -> UMap.union (fun _ _ y -> Some y) x acc
          | _ -> acc
        end
      | Cycle _ ->
        UMap.fold (fun input code acc ->
            let code = approx_cycle recpatch x code in
            UMap.add input code acc
          ) x UMap.empty
    in
    List.map (fun (input,code) -> {input; code} ) @@
    UMap.bindings @@ Map.fold undo map UMap.empty

end

type fault = Bind: 'a Failure.blocker * 'a i list -> fault

let fault =
  Fault.info ["solver"; "block" ]
    "Solver fault: major errors during analysis."
    (fun ppf (Bind(block, x),resolver) -> Format.fprintf ppf
        "Solver failure@?@[@<2> @[<0>@;%a@]@]"
        (Failure.pp_cycle block resolver) x
    )


(** Common functions *)

  let eval_bounded policy eval (unit:Unit.s) =
    let unit' = Unit.{ unit with code = Approx_parser.to_upper_bound unit.code } in
    let high = eval unit' in
    let low = eval unit in
    let lower, sign, upper = match low, high with
      | Ok (sg, lower), Ok(_sg, upper) -> lower, sg, upper
      | Ok(sg, lower) , Error _  -> lower, sg, lower
      | Error _, Ok(sg,upper)  -> Deps.empty, sg, upper
      | Error _, Error _ -> Deps.empty, Module.Sig.empty, Deps.empty
        (* something bad happened but we are already parsing
           problematic input *) in
    let elts m = Deps.paths m in
    let set m =
      let add {Deps.path; _ } = Paths.S.Set.add path in
      Deps.fold add m Paths.S.Set.empty in
    begin
      if elts upper = elts lower then
        Fault.raise policy Standard_faults.concordant_approximation unit.src
      else
        let diff =
          Paths.S.Set.elements @@ Paths.S.Set.diff (set upper) (set lower) in
        Fault.raise policy Standard_faults.discordant_approximation
          (unit.src, (Paths.S.Set.elements @@ set lower), diff)
    end;
    Unit.lift sign upper unit


let expand_epsilon resolved unit =
  let module M = Namespaced.Map in
  let add_epsilon_dep edge0 {Deps.path;edge;pkg;_} deps =
    if edge = Deps.Edge.Epsilon then
      Deps.update ~path ~edge:edge0 pkg deps
    else
      deps in
  let expand_dep {Deps.path;edge;pkg;_} deps =
    let deps = Deps.update ~path ~edge pkg deps in
    match Paths.P.Map.find_opt pkg resolved with
    | None -> deps
    | Some ancestor ->
      Deps.fold (add_epsilon_dep edge) (Unit.deps ancestor) deps in
  let deps = Deps.fold expand_dep (Unit.deps unit) Deps.empty in
  Unit.update deps unit

(* shortcut ε expansion when not needed *)
let expand_and_add expand  =
  if expand then
    fun resolved (unit:Unit.r) ->
      Paths.P.Map.add unit.src (expand_epsilon resolved unit) resolved
  else
    fun resolved (unit:Unit.r) ->
      Paths.P.Map.add unit.src unit resolved

module Make(Envt:Stage.envt)(Param:Stage.param)
    (Eval: Stage.outliner with type envt := Envt.t) =
struct
  open Unit

  type state = { resolved: Unit.r Paths.P.map;
                 env: Envt.t;
                 pending: Eval.on_going i list;
                 postponed: Unit.s list
               }
  let eq x y =
    x.resolved = y.resolved && x.pending = y.pending
    && x.postponed = y.postponed && Envt.eq x.env y.env

  let start env (units:Unit.s list) =
    List.fold_left (fun state (u:Unit.s) ->
        match u.precision with
        | Exact ->
          { state with pending = make Eval.initial u :: state.pending }
        | Approx ->
          let mock = Module.mockup u.path.name ~path:u.src in
          let env = Envt.add_unit state.env
              ~namespace:u.path.namespace u.path.name (Module.md mock) in
          { state with postponed = u:: state.postponed; env }
      )
      { postponed = [];
        env;
        pending = [];
        resolved = Paths.P.Map.empty } units

  let compute_more env (u:_ i) =
    Eval.next ~pkg:u.input.src env u.code

  let expand_and_add = expand_and_add Param.epsilon_dependencies

  let eval ?(learn=true) state unit =
    match compute_more state.env unit with
    | Ok (sg,deps) ->
      let env =
        if learn then begin
          let input = unit.input in
          let md = let open Module in
            create
              ~origin:(Unit {source=input.src; path=Namespaced.flatten input.path})
              input.path.name sg in
          Envt.add_unit ~namespace:input.path.namespace state.env
            input.path.name
            (Module.M md)
        end
        else
          state.env
      in
      let unit = Unit.lift sg deps unit.input in
      { state with env; resolved = expand_and_add state.resolved unit }
    | Error code ->
      let unit = { unit with code } in
      { state with pending = unit :: state.pending }

  let eval_bounded core =
    eval_bounded Param.policy
      (fun unit -> compute_more core @@ make Eval.initial unit)

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
      begin match resolve_dependencies ~learn:false
          @@ start env ml with
        | Ok(_, ml) -> Ok { ml; mli }
        | Error state -> Error( `Ml ( mli, state) )
      end
    | Error s -> Error (`Mli s)

  let alias_resolver state y path =
      let env = Envt.extend state.env y in
      match Envt.resolve_alias path env with
      | Some x -> x
      | None -> { name = List.hd path; namespace = [] }

  let blocker = Eval.block

  let approx_and_try_harder state  =
    let pending =
      Failure.approx_and_try_harder Eval.recursive_patching Eval.block
        (alias_resolver state) state.pending  in
    { state with pending }

  let solve core (units: _ Unit.pair) =
    let rec solve_harder ancestors state =
      match ancestors with
      | _ :: g :: _  when eq state g -> exit 2
      | _ ->
      match resolve_dependencies ~learn:true state with
      | Ok (e,l) -> e, l
      | Error state ->
        Fault.raise Param.policy fault
          (Bind(Eval.block,state.pending), alias_resolver state);
        solve_harder (state :: ancestors)
        @@ approx_and_try_harder state in
    let env, mli = solve_harder [] @@ start core units.mli in
    let _, ml = solve_harder [] @@ start env units.ml in
    {Unit.ml;mli}

end



module Directed(Envt:Stage.envt)(Param:Stage.param)
    (Eval: Stage.outliner with type envt := Envt.t) =
struct
  open Unit

  type gen = Namespaced.t -> Unit.s option Unit.pair

  type state = {
    gen: gen;
    resolved: Unit.r Paths.P.map;
    learned: Namespaced.Set.t;
    not_ancestors: Namespaced.Set.t;
    pending: Eval.on_going i list Namespaced.Map.t;
    env: Envt.t;
    postponed: Unit.s list;
    roots: Namespaced.t list
  }

  let get name state =
    (* Invariant name ∈ state.pending, except when looking at a new seed *)
    List.hd @@ Mp.find name state.pending

  let remove name pending =
    match Mp.find name pending with
    | _ :: (b :: _ as q) -> Some b, Mp.add name q pending
    | [_]| [] -> None, Mp.remove name pending
    | exception Not_found ->
      (* happens when trying to remove itself due to
         a second round the same unit after encoutering an approximated module *)
      None, pending

  let add_pending i state =
    let l = Option.default [] @@ Mp.find_opt i.input.path
        state.pending in
    { state with
      pending = Mp.add i.input.path ( i :: l ) state.pending
    }

  let update_pending i state =
    let pending =
    match Mp.find i.input.path state.pending with
    | _ :: q -> Mp.add i.input.path (i::q) state.pending
    | [] -> Mp.add i.input.path [i] state.pending in
    { state with pending }

  let compute state (i: _ i) =
    Eval.next ~pkg:i.input.src state.env i.code

  let add_unit (u:Unit.s) state =
    match u.precision with
    | Exact -> add_pending (make Eval.initial u) state
    | Approx ->
      let mockup = Module.(md @@ mockup ~path:u.src u.path.name) in
      let env =
        Envt.add_unit state.env ~namespace:u.path.namespace u.path.name mockup in
      { state with env; postponed = u :: state.postponed }

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


  let alias_resolver state y path =
    let env' = Envt.extend state.env y in
    match Envt.resolve_alias path env' with
    | None -> Namespaced.make (List.hd path)
    | Some name -> name

  let blocker = Eval.block

  let rec eval_depth state i =
    let path, src = i.input.path, i.input.src in
    let not_ancestors = Namespaced.Set.add path state.not_ancestors in
    let state = { state with not_ancestors } in
    match compute state i with
    | Ok (sg, deps) ->
      let more, pending = remove path state.pending in
      let state = { state with pending } in
      let state =
        if Namespaced.Set.mem path state.learned then
          state
        else
          let origin =
            Module.Origin.Unit {source=src; path=Namespaced.flatten path} in
          let md = Module.md @@
            Module.create ~origin path.name sg in
          let env =
            Envt.add_unit state.env ~namespace:path.namespace path.name md in
          { state with env;
                       learned = Namespaced.Set.add path state.learned
          } in
      let state =
        { state with
          resolved = expand_and_add state.resolved @@ Unit.lift sg deps i.input;
          not_ancestors =
            Namespaced.Set.remove path state.not_ancestors
        } in
      Option.(more >>| eval_depth state >< Ok state )
    | Error m2l ->
      (* first, we update the work-in-progress map *)
      let state = update_pending { i with code = m2l } state in
      (* we check the first missing module *)
      match Eval.block m2l with
      | None ->
        assert false (* we failed to eval the m2l code due to something *)
      | Some { Loc.data = y, path; _ } ->
        (*       debug "blocked at :%a@ %a" Paths.S.pp path M2l.pp m2l;*)
        let first_parent = alias_resolver state y path in
        debug "first parent:%a" Namespaced.pp first_parent;
        (* are we cycling? *)
        if Namespaced.Set.mem first_parent state.not_ancestors then
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
                go_on state (make Eval.initial u)
              else
                go_on state i

  let rec eval state i=
    let open Mresult.Ok in
    (eval_depth state i)
    >>=  (fun state ->
        if Mp.cardinal state.pending = 0 then
          Ok state
        else
          state.pending |> Mp.choose |> snd |> List.hd |> eval state
      )

  let eval_post state =
    let compute (u:Unit.s) = compute state @@ make Eval.initial u in
    let rec eval_more status state =
      match state.postponed with
      | [] -> status, state
      | u :: postponed ->
        let state =
          { (add_pending (make Eval.initial u) state) with postponed } in
      match u.precision with
      | Approx ->
        let r = eval_bounded Param.policy compute u in
        status, { state with
          resolved = expand_and_add state.resolved r
        }
      | Exact ->
        let state = add_pending (make Eval.initial u) state in
        match eval state (make Eval.initial u) with
        | Ok s -> eval_more status s
        | Error s -> eval_more false s
    in
    eval_more true state

  let wip state =
    List.flatten @@ List.map snd @@ Mp.bindings state.pending

  let end_result state =
    state.env, List.map snd @@ Paths.P.Map.bindings state.resolved


  let generator load_file files =
    let (++) = Unit.adder List.cons in
    let add_g (k,x,n) g =
      g ++ (k.Read.kind, (k, x, n) ) in
    let add m ((_, _x, path ) as f) =
      let g = Option.default {Unit.ml = []; mli=[]}
        @@ Mp.find_opt path m in
      Mp.add path (add_g f g) m in
    let m = List.fold_left add Mp.empty files in
    let convert k l =
      match l with
      | [] -> None
      | [a] -> Some a
      | a :: _  ->
        Fault.raise Param.policy Standard_faults.local_module_conflict
          (k, List.map (fun (_k,x,_n) -> Paths.P.local x) l);
        Some a in
    let convert_p (k, p) = k, Unit.unimap (convert k) p
    in
    let m = List.fold_left (fun acc (k,x) -> Mp.add k x acc)
        Mp.empty
      @@ List.map convert_p @@ Mp.bindings m in
    fun name ->
      Mp.find_opt name m
      |> Option.default {Unit.ml = None; mli = None}
      |> Unit.unimap (Option.fmap load_file)

  let start loader files env roots =
    debug "starting env:%a" Envt.pp env;
    let gen = generator loader files in
    {
      gen;
      resolved = Paths.P.Map.empty ;
      learned = Namespaced.Set.empty;
      not_ancestors= Namespaced.Set.empty;
      pending= Mp.empty;
      env;
      postponed= [];
      roots
    }

  let eq st1 st2 =
    st1.resolved = st2.resolved
 (*   && st1.pending = st2.pending
      && st1.env = st2.env *)

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
      Eval.recursive_patching Eval.block (alias_resolver state)
    |> List.fold_left (flip add_pending)
      { state with pending = Mp.empty;
                   not_ancestors = Namespaced.Set.empty }

  let solve loader files core seeds =
    let rec solve_harder ancestors state =
      match ancestors with
      | _ :: g :: _ when eq g state -> exit 2
      | _ ->
      match solve_once state with
      | Ok (e,l) -> e, l
      | Error s ->
        Fault.raise Param.policy fault
          (Bind(Eval.block, wip s), alias_resolver s);
        solve_harder (s :: ancestors) @@ approx_and_try_harder s in
    solve_harder [] @@ start loader files core seeds

  type entry = Read.kind * string * Namespaced.t
  type loader = entry -> Unit.s

end
