
module Make(Envt:Interpreter.envt_with_deps)(Param:Interpreter.param) = struct
  open Unit

  module Eval = Interpreter.Make(Envt)(Param)

  let prefilter env units =
    List.fold_left (fun (postponed, env, direct) u ->
        match u.precision with
        | Exact -> (postponed,env, u :: direct)
        | Approx ->
          let fictious =
            { Module.name = u.name;
              origin = Unit u.path;
              precision = Module.Precision.Unknown;
              args = [];
              signature = Module.Sig.empty
            } in
          u::postponed, Envt.add_module env fictious, direct
      )
      ([],env,[]) units

  let compute_more env unit =
    let result = Eval.m2l env unit.code in
    let deps = Envt.deps env in
    Envt.reset_deps env;
    deps, result

  exception Cycle of Envt.t * u list

  let eval ?(learn=true) (finished, core, rest) unit =
    let open M2l in
    match compute_more core unit with
    | deps, Ok (_,sg) ->
      let core =
        if learn then begin
          let md = Module.(create ~origin:(Unit unit.path)) unit.name sg in
          Envt.add_module core md
        end
        else
          core
      in
      let deps = Pkg.Set.union unit.dependencies deps in
      let unit = { unit with code = [Defs (Definition.sg_bind sg)];
                             dependencies = deps } in
      (unit :: finished, core, rest )
    | deps, Error code ->
      let deps = Pkg.Set.union unit.dependencies deps in
      let unit = { unit with dependencies = deps; code } in
      finished, core, unit :: rest

  let eval_bounded core unit =
    let open M2l in
    let unit' = Unit.{ unit with code = Approx_parser.to_upper_bound unit.code } in
    let r, r' = compute_more core unit, compute_more core unit' in
    let lower, upper = fst r, fst r' in
    let code = match snd r, snd r' with
      | _ , Ok (_,sg)
      | Ok(_,sg) , Error _  -> [Defs (Definition.sg_bind sg)]
      | Error _, Error _ -> unit.code
        (* something bad happened but we are already parsing problematic
           input *) in
    let elts = Paths.P.Set.elements in
    if elts upper = elts lower then
      Warning.log "Approximate parsing of %a.\n\
                   However, lower and upper bound agreed upon dependencies."
        Paths.P.pp unit.path
    else
      Warning.log "Approximate parsing of %a.\n\
                   Computed dependencies: at least {%a}, maybe: {%a}"
        Paths.P.pp unit.path
        Pp.(list string) (List.map Paths.P.module_name @@ elts lower)
        Pp.(list string) ( List.map Paths.P.module_name @@ elts
                           @@ Paths.P.Set.diff upper lower);
    { unit with dependencies = upper; code }


  let resolve_dependencies_main ?(learn=true) core units =
    let rec resolve alert env solved units =
      let solved, env, units' =
        List.fold_left (eval ~learn) (solved,env,[]) units in
      match List.length units' with
      | 0 -> env, solved
      | n when n = List.length units ->
        if alert then
            raise @@ Cycle (env, units)
        else resolve true env solved units'
      | _ ->
        resolve false env solved units' in
    resolve false core [] units

  let resolve_dependencies ?(learn=true) core units =
    let postponed, core, units = prefilter core units in
    let core, units = resolve_dependencies_main ~learn core units in
    let units' = List.map (eval_bounded core) postponed in
    core, units' @ units

  let resolve_split_dependencies env {ml; mli} =
    let env, mli = resolve_dependencies env mli in
    let _, ml = resolve_dependencies ~learn:false env ml in
    { ml; mli }
end


module Failure = struct
  open Unit
  type status =
    | Cycle of Name.t
    | Extern of Name.t
    | Depend_on of Name.t
    | Internal_error

  let analysis sources =
    let m = List.fold_left (fun m u -> Name.Map.add u.name (u, ref None) m )
        Name.Map.empty sources in
    let s = Name.Set.of_list @@ List.map (fun x -> x.name) @@ sources
    in
    let rec track s map (u,r) =
      let s = Name.Set.remove u.name s in
      let update r = s, Name.Map.add u.name (u,r) map in
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
              let map = Name.Map.add u'.name (u',r) map in
              track s map (u',r)
            end
            else begin
              r:= Some (Cycle u'.name);
              s, map
            end
          | Some (Depend_on name| Cycle name) ->
            (r := Some (Depend_on name); s, map)
          | Some (Extern _ | Internal_error ) ->
            (r := Some (Depend_on u'.name); s, map)


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
    let name u = u.name in
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
    let map = analysis sources in
    let cmap = categorize map in
    let cmap = normalize map cmap in
    pp map ppf cmap

end
