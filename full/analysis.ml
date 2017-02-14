module M = Module

type param = {
  epsilon_dependencies:bool;
  transparent_aliases: bool;
  transparent_extension_nodes: bool;
  policy: Fault.Policy.t;
  precomputed_libs: Name.set;
  closed_world: bool;
  sig_only:bool;
}



(** Basic files reading *)
let (%) f g x = f @@ g x

let open_within opens unit =
  List.fold_right (fun m (unit:Unit.s) ->
      match m with
      | [root] when unit.name = root -> unit
      | m -> { unit with code = (M2l.Build.ghost @@ M2l.Open m) :: unit.code }
    ) opens unit

type ('a,'b) either = Left of 'a | Right of 'b

let split either =
  let rec split either ( (l,r) as t) = function
    | [] -> t
    | a :: q ->
      q |> split either
        ( match either a with
          | Left a -> a :: l, r
          | Right a -> l, a :: r
        )
  in
  split either ([],[])


let info_split (io:Io.reader) = function
  | {Common.kind=Signature; _ }, f -> Right (io.sign f)
  | {Common.kind=Implementation;format} ,f ->
    Left ({ Read.kind = Structure; format}, f )
  | {Common.kind=Interface;format} ,f -> Left ({ Read.kind=Signature;format}, f )

let pair_split l =
  let folder (pair: _ Unit.pair) (x:Unit.s) =
    match x.kind with
    | M2l.Structure -> { pair with ml = x :: pair.ml }
    | Signature -> { pair with mli = x :: pair.mli } in
  List.fold_left folder {ml=[];mli=[]} l

(** organisation **)
let pre_organize io files =
  let units, signatures = split (info_split io) files in
  let signatures =
    List.flatten @@ Option.List'.filter signatures in
  units, signatures

let load_file (io:Io.reader) policy sig_only opens (info,file) =
  let filter_m2l (u: Unit.s) = if sig_only then
      { u with Unit.code = M2l.Sig_only.filter u.code }
    else
      u in
  file
  |> io.m2l policy info
  |> filter_m2l
  |> open_within opens


let log_conflict policy proj (path, units) =
  Fault.handle policy Standard_faults.local_module_conflict
    (Paths.S.module_name path)
  @@ List.map proj units


let organize io policy sig_only opens files =
  let units, signatures = pre_organize io files in
  let units = List.map (load_file io policy sig_only opens) units in
  let units, errs = Unit.Groups.Unit.(split % group) @@ pair_split units in
  List.iter (log_conflict policy @@ fun (u:Unit.s) -> u.path ) errs;
  units, signatures


let stdlib_pkg s l = match s with
  | "stdlib" -> Stdlib.signature :: l
  | "unix" -> Std_unix.signature :: l
  | "bigarray" -> Std_bigarray.signature :: l
  | "dynlink" -> Std_dynlink.signature :: l
  | "graph" -> Std_graph.signature :: l
  | "num" -> Std_num.signature :: l
  | "threads" -> Std_threads.signature :: l
  | _ -> l


let base_env io signatures =
  let (++) = Module.Def.merge in
  Envts.Base.start @@
  List.fold_left (++) Module.Def.empty signatures
  ++ io.Io.env

(** Environment *)
type 'a envt_kind = (module Outliner.envt_with_deps with type t = 'a)
type envt = E: 'a envt_kind * 'a -> envt

let start_env io param libs signatures fileset =
  let signs = List.map Module.Sig.flatten
    @@ Name.Set.fold stdlib_pkg param.precomputed_libs [] in
  let base = base_env io signs in
  let base = List.fold_left Envts.Base.add_unit base signatures in
  let layered = Envts.Layered.create libs fileset base in
  let traced = Envts.Trl.extend layered in
  if not param.closed_world then
    E ((module Envts.Tr: Outliner.envt_with_deps with type t = Envts.Tr.t ) ,
       Envts.Tr.start traced fileset )
  else
    E ( (module Envts.Trl: Outliner.envt_with_deps with type t = Envts.Trl.t),
        traced
      )

(** Solver step *)

let lift p =
  (module struct
    let policy = p.policy
    let epsilon_dependencies = p.epsilon_dependencies
    let transparent_extension_nodes = p.transparent_extension_nodes
    let transparent_aliases = p.transparent_aliases
  end
  : Outliner.param )

let solve param (E((module Envt), core)) (units: _ Unit.pair) =
  let module S = Solver.Make(Envt)((val lift param)) in
  S.solve core units

let solve_from_seeds seeds loader files param (E((module Envt), core)) =
  let module S = Solver.Directed(Envt)((val lift param)) in
  let gen = S.generator loader files in
  snd @@ S.solve gen core seeds

let remove_units invisibles =
  List.filter @@ function
    | { Unit.path = { Paths.Pkg.source=Local; file}; _ } ->
      not @@ Paths.S.Set.mem file invisibles
    | _ -> false


module Collisions = struct
  (** Check that there is no module name collisions with libraries and local files*)
  (** Note: no library/library collision detection*)

  let empty = Name.Map.empty

  (** add a new collision [path] to a map of collision [m]
      for a module name [name] *)
  let add name path m =
    let s = Option.default Paths.P.Set.empty @@ Name.Map.find_opt name m in
    Name.Map.add name (Paths.P.Set.add path s) m

  (** Compute local/libraries collisions *)
  let libs (task:Common.task) units =
    let module E = Envts.Layered in
    let env =
      let base = Envts.Base.empty in
      E.create task.libs Name.Set.empty base in
    let m = Name.Map.empty in
    List.fold_left (fun m (u:Unit.s) ->
        match E.find M.Module [u.name] env with
        | exception Not_found -> m
        | { main = { M.origin = Unit p; _ }; msgs= [] } ->
          (add u.name p @@ add u.name u.path m)
        | { msgs = _ :: _ ; _ }
        | { main = { M.origin = (Phantom _ |Arg|Submodule|First_class); _ }; _ }
          -> m

      ) m units

  (** Print error message for a given collision map *)
  let handle policy fault collisions =
    List.iter (fun (name,paths) ->
        Fault.handle policy fault
          name @@ Paths.P.Set.elements paths)
      (Name.Map.bindings collisions)

  (** Compute local/local collisions *)
  let local units =
    let potential_collisions, set =
    List.fold_left
      (fun (collisions, name_set) (u:Unit.s) ->
         add u.name u.path collisions, Name.Set.add u.name name_set
      )
      (empty,Name.Set.empty) units in
    Name.Map.filter (fun _k s -> Paths.P.Set.cardinal s > 1) potential_collisions,
    set

end





(** Analysis step *)
let main_std io param (task:Common.task) =
  let module F = Standard_faults in
  let units, signatures =
    organize io param.policy param.sig_only task.opens task.files in
  if not @@ Fault.is_silent param.policy F.module_conflict then
    Collisions.libs task units.mli
    |> Collisions.handle param.policy F.module_conflict;
  let collisions, file_set = Collisions.local units.mli in
  let () =
    if not @@ Fault.is_silent param.policy F.local_module_conflict then
      Collisions.handle param.policy F.local_module_conflict collisions in
  let e = start_env io param task.libs signatures file_set in
  let {Unit.ml; mli} = solve param e units in
  let ml = remove_units task.invisibles ml in
  let mli = remove_units task.invisibles mli in
  {Unit.ml;mli}

(** Analysis step *)
let main_seed io param (task:Common.task) =
  let units, signatures =
    pre_organize io task.files in
  let file_set = List.fold_left (fun s x ->
      Name.Set.add Paths.P.(module_name @@ local @@ snd x) s
    ) Name.Set.empty units in
  let load_file = load_file io param.policy param.sig_only task.opens in
  let e = start_env io param task.libs signatures file_set in
  let units = solve_from_seeds task.seeds load_file units param e in
  let units = remove_units task.invisibles units in
  let units = List.fold_left (fun (pair: _ Unit.pair) (u:Unit.r)->
      match u.kind with
      | Structure -> { pair with ml = u :: pair.ml }
      | Signature -> {pair with mli = u :: pair.mli }
    ) { ml=[]; mli=[]} units in
  let g, errs = Unit.Groups.R.(split % group) units in
  List.iter (log_conflict param.policy @@ fun (u:Unit.r) -> u.path) errs;
  g

let main io param (task:Common.task) =
  match task.seeds with
  | [] -> main_std io param task
  | _ -> main_seed io param task
