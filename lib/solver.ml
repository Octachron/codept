
type i = { input: Unit.s; code: M2l.t; deps: Paths.P.set }

module Failure = struct
  module Set = Set.Make(struct type t = i let compare = compare end)

  type status =
    | Cycle of Name.t
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
      | Some name' ->
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
              r:= Some (Cycle u'.input.name);
              s, map
            end
          | Some (Depend_on name| Cycle name) ->
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
        | Some x -> x
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
        | Cycle name as st -> fun set map ->
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
    Pp.string ppf name;
    if name <> start || first then
      let u = fst @@ Name.Map.find name map in
      match M2l.Block.m2l u.code with
      | None -> ()
      | Some next -> begin
          Pp.fp ppf " ⇒ ";
          pp_circular map start false ppf next
        end

  let pp_cat map ppf (st, units) =
    let name u = u.input.name in
    let names units = List.map name @@ Set.elements units in
    match st with
    | Internal_error -> Pp.fp ppf "@[ Internal error for units: {%a} @]"
                          Pp.(list ~sep:(s ", @ ") @@ string ) (names units)
    | Extern name -> Pp.fp ppf "Non-resolved external dependency.\n@[\
                                The following units {%a} depend on \
                                the unknown module \"%s\" @]"
                       Pp.(list ~sep:(s ", @ ") @@ string ) (names units)
                       name
    | Depend_on name ->
      Pp.fp ppf "Non-resolved internal dependency.@;\
                 The following modules {%a} depend on the unit \
                 \"%s\" that could not be resolved."
        Pp.(list ~sep:(s ", @ ") @@ string ) (names units)
        name
    | Cycle name ->  Pp.fp ppf "Circular dependencies: %a"
                        (pp_circular map name true) name
  (* Pp.(list ~sep:(s ", @ ") @@ pp ) (Set.elements units) *)

  let pp map ppf m =
    Pp.fp ppf "@[%a@]"
      Pp.(list ~sep:(s"@;") @@ pp_cat map ) (Map.bindings m)

  let pp_cycle ppf sources =
    let map, cmap = analyze sources in
    pp map ppf cmap

end

module Make(Envt:Interpreter.envt_with_deps)(Param:Interpreter.param) = struct
  open Unit

  module Eval = Interpreter.Make(Envt)(Param)

  let start_u input = { input; code = input.code; deps = Paths.P.Set.empty }

  type state = { resolved: Unit.r list;
                 env: Envt.t;
                 pending: i list;
                 postponed: Unit.s list
               }

  let start env (units:Unit.s list) =
    List.fold_left (fun state (u:Unit.s) ->
        match u.precision with
        | Exact -> { state with pending = start_u u :: state.pending }
        | Approx ->
          let fictious =
            Module.M { Module.name = u.name;
              origin = Unit u.path;
              precision = Module.Precision.Unknown;
              args = [];
              signature = Module.Sig.empty
                     } in
          { state with postponed = u:: state.postponed;
                       env = Envt.add_unit state.env fictious
          }
      )
      { postponed = []; env; pending = []; resolved = [] } units

  let compute_more env (u:i) =
    let result = Eval.m2l env u.code in
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

  let eval_bounded core (unit:Unit.s) =
    let unit' = Unit.{ unit with code = Approx_parser.to_upper_bound unit.code } in
    let r, r' = compute_more core @@ start_u unit,
                compute_more core @@ start_u unit' in
    let lower, upper = fst r, fst r' in
    let sign = match snd r, snd r' with
      | _ , Ok (_,sg)
      | Ok(_,sg) , Error _  ->  sg
      | Error _, Error _ -> Module.Sig.empty
        (* something bad happened but we are already parsing problematic
           input *) in
    let elts = Paths.P.Set.elements in
    if elts upper = elts lower then
      Fault.(handle Param.polycy concordant_approximation unit.path)
    else
      Fault.(handle Param.polycy discordant_approximation
        unit.path
        (List.map Paths.P.module_name @@ elts lower)
        ( List.map Paths.P.module_name @@ elts
          @@ Paths.P.Set.diff upper lower) );
    Unit.lift sign upper unit


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


  let approx_cycle set (i:i) =
    let mock (unit: Unit.s) =
            Module.create unit.name
              ~origin:(Unit unit.path)
              ~precision:Unknown
              Module.Sig.empty in
    let add_set def =
      Failure.Set.fold
        (fun n acc -> Definition.see (Module.M(mock n.input)) acc) set def in
    let code =
      match i.code with
      | M2l.Defs def :: q -> M2l.Defs (add_set def) :: q
      | code -> M2l.Defs (add_set Definition.empty) :: code
    in
    { i with code }

  let approx_and_try_harder state  =
    let open Failure in
    let cmap, map = analyze state.pending in
    let undo key x acc = match key with
      | Failure.Extern _ | Internal_error -> acc
      | Depend_on f ->
        begin
          match Failure.ancestor_status f cmap with
          | Cycle _ -> Set.union x acc
          | _ -> acc
        end
      | Cycle _ ->
        Set.elements x
        |> List.map (approx_cycle x)
        |> Set.of_list in
    let pending = Set.elements @@ Failure.Map.fold undo map Set.empty in
    { state with pending }


end
