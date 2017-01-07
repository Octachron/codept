module M = Module

type param = {
  transparent_aliases: bool;
  transparent_extension_nodes: bool;
  polycy: Fault.Polycy.t;
  no_stdlib:bool;
  closed_world: bool;
  sig_only:bool;
}

(** Basic files reading *)
let local = Paths.Pkg.local
let (%) f g x = f @@ g x

let open_within opens unit =
  List.fold_right (fun m (unit:Unit.s) ->
      match m with
      | [root] when unit.name = root -> unit
      | m -> { unit with code = (M2l.Build.ghost @@ M2l.Open m) :: unit.code }
    ) opens unit


(** organisation **)
let organize polycy sig_only opens files =
  let add_name m (_,n)  =  Name.Map.add (Read.name n) (local n) m in
  let m = List.fold_left add_name
      Name.Map.empty (files.Unit.ml @ files.mli) in
  let filter_m2l (u: Unit.s) = if sig_only then
      { u with Unit.code = M2l.Sig_only.filter u.code }
    else
      u in
  let units =
    Unit.unimap (List.map @@ fun (info,f) -> Unit.read_file polycy info f )
      files in
  let units = Unit.unimap (List.map @@ filter_m2l % open_within opens) units in
  let units = Unit.Groups.Unit.(split % group) units in
  units, m


let base_env signatures no_stdlib =
  let start =
    if no_stdlib then
      Envts.Base.empty
    else
      Envts.Base.start Stdlib.signature in
  List.fold_left Envts.Base.add_unit start signatures

(** Environment *)
type 'a envt_kind = (module Interpreter.envt_with_deps with type t = 'a)
type envt = E: 'a envt_kind * 'a -> envt

let start_env param {Common.libs; signatures; _} fileset filemap =
  let base = base_env signatures param.no_stdlib in
  let layered = Envts.Layered.create libs fileset base in
  let traced = Envts.Trl.extend layered in
  if not param.closed_world then
    E ((module Envts.Tr: Interpreter.envt_with_deps with type t = Envts.Tr.t ) ,
       Envts.Tr.start traced filemap )
  else
    E ( (module Envts.Trl: Interpreter.envt_with_deps with type t = Envts.Trl.t),
        traced
      )

(** Solver step *)

let lift { polycy; transparent_extension_nodes; transparent_aliases; _ } =
  (module struct
    let polycy = polycy
    let transparent_extension_nodes = transparent_extension_nodes
    let transparent_aliases = transparent_aliases
  end
  : Interpreter.param )



let solve param (E((module Envt), core)) (units: _ Unit.pair) =
  let module S = Solver.Make(Envt)((val lift param)) in
  let rec solve_harder state =
    match S.resolve_dependencies ~learn:true state with
    | Ok (e,l) -> e, l
    | Error state ->
      Fault.handle param.polycy Codept_polycy.solver_error state.pending;
      solve_harder @@ S.approx_and_try_harder state in
  let env, mli = solve_harder @@ S.start core units.mli in
  let _, ml = solve_harder @@ S.start env units.ml in
  {Unit.ml;mli}

let remove_units invisibles =
  List.filter @@ function
    | { Unit.path = { Paths.Pkg.source=Local; file}; _ } ->
      not @@ Paths.S.Set.mem file invisibles
    | _ -> false

(** Check that the starting environment e and the list of units does
    not contain module name collision *)
let check_env param (task:Common.task) units =
  let module E = Envts.Layered in
  let env =
    let base = base_env task.signatures param.no_stdlib in
    E.create task.libs Name.Set.empty base in
  let m = Name.Map.empty in
  let add name path m =
    let s = Option.default Paths.P.Set.empty @@ Name.Map.find_opt name m in
    Name.Map.add name (Paths.P.Set.add path s) m in
  let m = List.fold_left (fun m (u:Unit.s) ->
      match E.find M.Module [u.name] env with
      | exception Not_found -> m
      | Ok { M.origin = Unit p; _ } ->
        (add u.name p @@ add u.name u.path m)
      | Error _ | Ok { M.origin = (Arg|Submodule|First_class); _ } -> m

    ) m units in
  List.iter (fun (name,paths) ->
      Fault.handle param.polycy Codept_polycy.module_conflict
        name @@ Paths.P.Set.elements paths)
    (Name.Map.bindings m)

(** Analysis step *)
let main param (task:Common.task) =
  let units, filemap = organize param.polycy param.sig_only task.opens task.files in
  let files_set = units.mli
                  |> List.map (fun (u:Unit.s) -> u.name)
                  |> Name.Set.of_list in
  let () =
    (* check that no modules is defined twice *)
    check_env param task units.mli in
  let e = start_env param task files_set filemap in
  let {Unit.ml; mli} = solve param e units in
  let ml = remove_units task.invisibles ml in
  let mli = remove_units task.invisibles mli in
  {Unit.ml;mli}
