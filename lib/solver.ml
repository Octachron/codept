
module Deps = struct
  type t = Paths.P.set
  let empty = Paths.P.Set.empty
  let merge = Paths.P.Set.union
  let (+) = merge
end

type i = { input: Unit.s; code: M2l.t; deps: Deps.t  }
let make input = { input; code = input.code; deps = Paths.P.Set.empty }

module Failure = struct
  module Set = Set.Make(struct type t = i let compare = compare end)

  type status =
    | Cycle of Name.t M2l.with_location
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
      | Some { data = name'; loc } ->
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
        | Some { data = x; _ } -> x
        | None -> assert false
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


  let rec pp_circular map start first ppf (name: string M2l.with_location) =
    Pp.fp ppf "%s" name.M2l.data;
    if name.data <> start || first then
      let u = fst @@ Name.Map.find name.data map in
      match M2l.Block.m2l u.code with
      | None -> ()
      | Some next -> begin
          Pp.fp ppf " −(%a:%a)⟶@ " Paths.Pkg.pp u.input.path M2l.Loc.pp name.loc;
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
                        (pp_circular map name.data true) name

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
        (fun n acc -> Definition.see (mock n.input) acc) set def in
    let code =
      match i.code with
      | { data = M2l.Defs def; loc } :: q ->
        { M2l.data = M2l.Defs (add_set def); loc } :: q
      | code -> (M2l.Loc.nowhere @@ M2l.Defs (add_set Definition.empty)) :: code
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


(** Common functions *)

  let eval_bounded polycy eval (unit:Unit.s) =
    let unit' = Unit.{ unit with code = Approx_parser.to_upper_bound unit.code } in
    let r, r' = eval unit, eval unit' in
    let lower, upper = fst r, fst r' in
    let sign = match snd r, snd r' with
      | _ , Ok (_,sg)
      | Ok(_,sg) , Error _  ->  sg
      | Error _, Error _ -> Module.Sig.empty
        (* something bad happened but we are already parsing problematic
           input *) in
    let elts = Paths.P.Set.elements in
    if elts upper = elts lower then
      Fault.(handle polycy concordant_approximation unit.path)
    else
      Fault.(handle polycy discordant_approximation
        unit.path
        (List.map Paths.P.module_name @@ elts lower)
        ( List.map Paths.P.module_name @@ elts
          @@ Paths.P.Set.diff upper lower) );
    Unit.lift sign upper unit


module Make(Envt:Interpreter.envt_with_deps)(Param:Interpreter.param) = struct
  open Unit

  module Eval = Interpreter.Make(Envt)(Param)


  type state = { resolved: Unit.r list;
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
      { postponed = []; env; pending = []; resolved = [] } units

  let compute_more env (u:i) =
    let result = Eval.m2l u.input.path env u.code in
    let deps = Envt.deps env in
    Envt.reset_deps env;
    deps, result


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
      let deps = Pkg.Set.union unit.deps deps in
      let unit = Unit.lift sg deps unit.input in
      { state with env; resolved = unit :: state.resolved }
    | deps, Error code ->
      let deps = Pkg.Set.union unit.deps deps in
      let unit = { unit with deps; code } in
      { state with pending = unit :: state.pending }

  let eval_bounded core =
    eval_bounded Param.polycy (fun unit -> compute_more core @@ make unit)

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
      Ok (env, units' @ res )
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

end



module Directed(Envt:Interpreter.envt_with_deps)(Param:Interpreter.param) = struct
  open Unit

  module Eval = Interpreter.Make(Envt)(Param)


  type gen = Name.t -> Unit.s option Unit.pair

  type state = {
    gen: gen;
    resolved: Unit.r list;
    learned: Name.set;
    not_ancestors: Name.set;
    pending: i list Name.map;
    env: Envt.t;
    postponed: Unit.s list;
    roots: Name.t list
  }

  let get name state =
    (* Invariant name ∈ state.pending *)
    List.hd @@ Name.Map.find name state.pending

  let remove name pending =
    match Name.Map.find name pending with
    | _ :: (b :: _ as q) -> Some b, Name.Map.add name q pending
    | [_] as q -> None, Name.Map.add name q pending
    | [] -> None, Name.Map.remove name pending

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

  let rec eval state i =
    let name, path = i.input.name, i.input.path in
    let not_ancestors = Name.Set.add name state.not_ancestors in
    let state = { state with not_ancestors } in
    match compute state i with
    | deps, Ok(_,sg) ->
      let more, pending = remove name state.pending in
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
          resolved = Unit.lift sg deps i.input :: state.resolved;
          pending;
          not_ancestors = Name.Set.remove name state.not_ancestors
        } in
      Option.(more >>| eval state >< Ok state )
    | deps, Error m2l ->
      (* first, we update the work-in-progress map *)
      let state = update_pending { i with deps; code = m2l } state in
      (* we check the first missing module *)
      match M2l.Block.m2l m2l with
      | None ->
        assert false (* we failed to eval the m2l code due to something *)
      | Some { M2l.data = first_parent; _ } ->
      (* are we cycling? *)
        if Name.Set.mem first_parent state.not_ancestors then
          Error state
        else
          let go_on state i' =
            match eval state i' with
            | Ok state -> eval state i
            | Error state -> Error state in
          (* do we have an unit corresponding to this unit name *)
          match get first_parent state with
          | u -> go_on state u
          | exception Not_found ->
            match state.gen first_parent with
            | {mli=None; ml = None} -> Error state
            | ({ mli = Some u; _ } | { ml = Some u; mli = None } as pair) ->
              go_on (add_pair state pair) (make u)

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
        let r = eval_bounded Param.polycy compute u in
        status, { state with
          resolved = r :: state.resolved
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
    state.env, state.resolved

  let start gen env roots =
    {
      gen;
      resolved = [] ;
      learned = Name.Set.empty;
      not_ancestors= Name.Set.empty;
      pending= Name.Map.empty;
      env;
      postponed= [];
      roots
    }

  let solve state =
    let rec solve_for_roots state = function
      | [] ->
        let ok, state = eval_post state in
        if ok then
          Ok (end_result state)
        else
          Error state
      | a :: q ->
        let state = add_name state a in
        let u = get a state in
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

end
