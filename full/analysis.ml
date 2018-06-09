module M = Module
module Nms = Namespaced

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
      | [root] when unit.path = { name = root; namespace = [] } ->
        unit
      | m ->
        { unit with
          code = (M2l.Build.ghost @@ M2l.Open m) :: unit.code }
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

let default_path f =
  Option.default (Namespaced.of_filename f)


let info_split (io:Io.reader) = function
  | {Common.kind=Signature; _ }, f, _ -> Right (io.sign f, f)
  | {Common.kind=Implementation;format}, f, n ->
    Left ({ Read.kind = Structure; format}, f, default_path f n )
  | {Common.kind=Interface;format}, f, n ->
    Left ({ Read.kind=Signature;format}, f, default_path f n )

let pair_split l =
  let folder (pair: _ Unit.pair) (x:Unit.s) =
    match x.kind with
    | M2l.Structure -> { pair with ml = x :: pair.ml }
    | Signature -> { pair with mli = x :: pair.mli } in
  List.fold_left folder {ml=[];mli=[]} l

(** organisation **)
let signature_error policy = function
  | Ok x, _ -> Some x
  | Error e, filename ->
    Standard_faults.schematic_errors policy (filename,"sig",e);
    None

let pre_organize policy io files =
  let units, signatures = split (info_split io) files in
  let signatures =
    List.flatten @@ Option.List'.filter
    @@ List.map (signature_error policy) signatures in
  units, signatures

let load_file (io:Io.reader) policy sig_only opens (info,file,n) =
  let filter_m2l (u: Unit.s) = if sig_only then
      { u with Unit.code = M2l.Sig_only.filter u.code }
    else
      u in
  io.m2l policy info file n
  |> filter_m2l
  |> open_within opens


let log_conflict policy proj (path, units) =
  Fault.handle policy Standard_faults.local_module_conflict
    (Namespaced.of_path path)
  @@ List.map proj units


let organize io policy sig_only opens files =
  let units, signatures = pre_organize policy io files in
  let units = List.map (load_file io policy sig_only opens) units in
  let units, errs = Unit.Groups.Unit.(split % group) @@ pair_split units in
  List.iter (log_conflict policy @@ fun (u:Unit.s) -> u.src ) errs;
  units, signatures


let stdlib_pkg s l = match s with
  | "unix" -> Std_unix.modules :: l
  | "bigarray" -> Std_bigarray.modules :: l
  | "dynlink" -> Std_dynlink.modules :: l
  | "graph" -> Std_graph.modules :: l
  | "num" -> Std_num.modules :: l
  | "threads" -> Std_threads.modules :: l
  | _ -> l

let prefixed_stdlib () =
  let modules = Stdlib_data.modules in
  if Version.(major,minor) < (4,7) then
    M.Dict.of_list [M.(Namespace {name="Stdlib"; modules })]
  else
    modules

let base_sign io signatures =
  let (++) dict m = Name.Map.add (Module.name m) m dict in
  let m = List.fold_left (++) Name.Map.empty signatures in
  Name.Map.union' m io.Io.env

(** Environment *)
type 'a envt_kind = (module Outliner.envt_with_deps with type t = 'a)
type envt = E: 'a envt_kind * 'a -> envt

let start_env io param libs signatures fileset =
  let signs = Name.Set.fold stdlib_pkg param.precomputed_libs [] in
  let signs = List.flatten
    @@ List.map( fun x -> List.map snd @@ Name.Map.bindings x) signs in
  let base_sign = base_sign io signs in
  let implicits =
    if Name.Set.mem "stdlib" param.precomputed_libs then
      [["Stdlib"], prefixed_stdlib ()]
    else
      [] in
  let env =
    Envt.start
      ~open_approximation:(not param.closed_world)
      ~libs
      ~namespace:fileset
      ~implicits
      base_sign
  in
  let add u env = Envt.Core.add_unit u env in
  let env = List.fold_left add env signatures in
    E ((module Envt.Core: Outliner.envt_with_deps with type t = Envt.Core.t ) ,
       env )

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

let solve_from_seeds seeds loader files param
    (E((module Envt), core)) =
  let module S = Solver.Directed(Envt)((val lift param)) in
  snd @@ S.solve loader files core seeds

let remove_units invisibles =
  List.filter @@ function
    | { Unit.src = { Paths.Pkg.source=Local; file}; _ } ->
      not @@ Paths.S.Set.mem file invisibles
    | _ -> false


module Collisions = struct
  (** Check that there is no module name collisions with libraries and local files*)
  (** Note: no library/library collision detection*)

  let empty = Nms.Map.empty

  (** add a new collision [path] to a map of collision [m]
      for a module name [name] *)
  let add name path m =
    let s = Option.default Paths.P.Set.empty
      @@ Nms.Map.find_opt name m in
    Nms.Map.add name (Paths.P.Set.add path s) m

  (** Compute local/libraries collisions *)
  let libs (task:Common.task) units =
    let env =
      Envt.start
        ~open_approximation:false
        ~libs:task.libs
        ~implicits:[]
        ~namespace:[]
        Module.Dict.empty in
    let m = Nms.Map.empty in
    List.fold_left (fun m (u:Unit.s) ->
        match Envt.Core.find M.Module (Nms.flatten u.path) env with
        | exception Not_found -> m
        | { main = M { M.origin = Unit p; _ }; msgs= [] } ->
          (add u.path p.source @@ add u.path u.src m)
        | { msgs = _ :: _ ; _ }
        | { main = Namespace _ | M { M.origin =
                       (Phantom _ |Arg|Submodule|First_class); _ }; _ }
          -> m

      ) m units

  (** Print error message for a given collision map *)
  let handle policy fault collisions =
    List.iter (fun (name,paths) ->
        Fault.handle policy fault
          name @@ Paths.P.Set.elements paths)
      (Nms.Map.bindings collisions)

  (** Compute local/local collisions *)
  let local units =
    let potential_collisions =
    List.fold_left
      (fun collisions (u:Unit.s) -> add u.path u.src collisions )
      empty units in
    Nms.Map.filter (fun _k s -> Paths.P.Set.cardinal s > 1)
      potential_collisions

end





(** Analysis step *)
let main_std io param (task:Common.task) =
  let module F = Standard_faults in
  let units, signatures =
    organize io param.policy param.sig_only task.opens task.files in
  if not @@ Fault.is_silent param.policy F.module_conflict then
    Collisions.libs task units.mli
    |> Collisions.handle param.policy F.module_conflict;
  let collisions = Collisions.local units.mli in
  let namespace = List.map (fun (u:Unit.s) -> u.path) units.mli in
  let () =
    if not @@ Fault.is_silent param.policy F.local_module_conflict then
      Collisions.handle param.policy F.local_module_conflict collisions in
  let e = start_env io param task.libs signatures namespace in
  let {Unit.ml; mli} = solve param e units in
  let ml = remove_units task.invisibles ml in
  let mli = remove_units task.invisibles mli in
  {Unit.ml;mli}

(** Analysis step *)
let main_seed io param (task:Common.task) =
  let units, signatures =
    pre_organize param.policy io task.files in
  let file_list = List.map (fun (_k,_x,p) -> p) units in
  let load_file = load_file io param.policy param.sig_only task.opens in
  let e = start_env io param task.libs signatures file_list in
  let units = solve_from_seeds task.seeds load_file units param e in
  let units = remove_units task.invisibles units in
  let units = List.fold_left (fun (pair: _ Unit.pair) (u:Unit.r)->
      match u.kind with
      | Structure -> { pair with ml = u :: pair.ml }
      | Signature -> {pair with mli = u :: pair.mli }
    ) { ml=[]; mli=[]} units in
  let g, errs = Unit.Groups.R.(split % group) units in
  List.iter
    (log_conflict param.policy @@ fun (u:Unit.r) -> u.src) errs;
  g

let main io param (task:Common.task) =
  match task.seeds with
  | [] -> main_std io param task
  | _ -> main_seed io param task
