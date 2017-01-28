module M = Module

type param = {
  transparent_aliases: bool;
  transparent_extension_nodes: bool;
  policy: Fault.Policy.t;
  precomputed_libs: Name.set;
  closed_world: bool;
  sig_only:bool;
}

type io = {
  sign: string -> Module.t list option;
  m2l: Fault.Policy.t -> Read.kind -> string -> Unit.s;
  findlib: Common.task -> Findlib.query -> Common.task ;
  env: Module.Def.t
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

let parse_sig lexbuf=
  Sexp.( (list Module.sexp).parse )
  @@ Sexp_parse.many Sexp_lex.main
  @@ lexbuf

let read_sigfile filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let sigs = parse_sig lexbuf in
  close_in chan;
  sigs

let info_split io = function
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

let load_file io policy sig_only opens (info,file) =
  let filter_m2l (u: Unit.s) = if sig_only then
      { u with Unit.code = M2l.Sig_only.filter u.code }
    else
      u in
  file
  |> io.m2l policy info
  |> filter_m2l
  |> open_within opens


let organize io policy sig_only opens files =
  let units, signatures = pre_organize io files in
  let units = List.map (load_file io policy sig_only opens) units in
  let units = Unit.Groups.Unit.(split % group) @@ pair_split units in
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
  ++ io.env
(** Environment *)
type 'a envt_kind = (module Interpreter.envt_with_deps with type t = 'a)
type envt = E: 'a envt_kind * 'a -> envt

let start_env io param libs signatures fileset =
  let signs = List.map Module.Sig.flatten
    @@ Name.Set.fold stdlib_pkg param.precomputed_libs [] in
  let base = base_env io signs in
  let base = List.fold_left Envts.Base.add_unit base signatures in
  let layered = Envts.Layered.create libs fileset base in
  let traced = Envts.Trl.extend layered in
  if not param.closed_world then
    E ((module Envts.Tr: Interpreter.envt_with_deps with type t = Envts.Tr.t ) ,
       Envts.Tr.start traced fileset )
  else
    E ( (module Envts.Trl: Interpreter.envt_with_deps with type t = Envts.Trl.t),
        traced
      )

(** Solver step *)

let lift { policy; transparent_extension_nodes; transparent_aliases; _ } =
  (module struct
    let policy = policy
    let transparent_extension_nodes = transparent_extension_nodes
    let transparent_aliases = transparent_aliases
  end
  : Interpreter.param )

let oracle policy load_file files =
  let (++) = Unit.adder List.cons in
  let add_g (k,x) g = g ++ (k.Read.kind, (k,x) ) in
  let add (s,m) ((_,x) as f) =
    let name =  Paths.P.( module_name @@ local x) in
    let g = Option.default {Unit.ml = []; mli=[]} @@ Name.Map.find_opt name m in
    Name.Set.add name s, Name.Map.add name (add_g f g) m in
  let s, m = List.fold_left add Name.(Set.empty, Map.empty) files in
  let convert k l =
    match l with
    | [] -> None
    | [a] -> Some a
    | a :: _  ->
      Fault.handle policy Codept_policies.module_conflict k
        @@ List.map (Paths.P.local % snd) l;
      Some a in
  let convert_p (k, p) = k, Unit.unimap (convert k) p
  in
  let m = List.fold_left (fun acc (k,x) -> Name.Map.add k x acc) Name.Map.empty
    @@ List.map convert_p @@ Name.Map.bindings m in
  s,
  fun name ->
    Name.Map.find_opt name m
    |> Option.default {Unit.ml = None; mli = None}
    |> Unit.unimap (Option.fmap load_file)

let solve param (E((module Envt), core)) (units: _ Unit.pair) =
  let module S = Solver.Make(Envt)((val lift param)) in
  let rec solve_harder state =
    match S.resolve_dependencies ~learn:true state with
    | Ok (e,l) -> e, l
    | Error state ->
      Fault.handle param.policy Codept_policies.solver_error state.pending;
      solve_harder @@ S.approx_and_try_harder state in
  let env, mli = solve_harder @@ S.start core units.mli in
  let _, ml = solve_harder @@ S.start env units.ml in
  {Unit.ml;mli}

let solve_from_seeds seeds gen param (E((module Envt), core)) =
  let module S = Solver.Directed(Envt)((val lift param)) in
  let rec solve_harder state =
    match S.solve state with
    | Ok (e,l) -> e, l
    | Error s ->
      Fault.handle param.policy Codept_policies.solver_error (S.wip s);
      solve_harder @@ S.approx_and_try_harder s in
  snd @@ solve_harder @@ S.start gen core seeds


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
  let handle policy collisions =
    List.iter (fun (name,paths) ->
        Fault.handle policy Codept_policies.module_conflict
          name @@ Paths.P.Set.elements paths)
      (Name.Map.bindings collisions)

  (** Compute local/local collisions *)
  let local collisions units =
    List.fold_left
      (fun (collisions, name_set) (u:Unit.s) ->
         if Name.Set.mem u.name name_set then
           (add u.name u.path collisions, name_set)
         else
           (collisions, Name.Set.add u.name name_set)
      ) (collisions,Name.Set.empty) units

end


module Findlib = struct
(** Small import *)
let add_ppx ppx =
  let first_ppx = Compenv.first_ppx in
  first_ppx := ppx :: !first_ppx

let lib (task:Common.task ref) f =
  task := { !task with libs = f :: (!task).libs }

let expand task query =
  let task = ref task in
  let result = Findlib.process query in
  Clflags.preprocessor := result.pp;
  List.iter (lib task) result.libs; List.iter add_ppx result.ppxs;
  !task
end

let  direct_io = {
  sign = read_sigfile;
  m2l = Unit.read_file;
  env = Module.Def.empty;
  findlib = Findlib.expand
}

(** Analysis step *)
let main_std io param (task:Common.task) =
  let units, signatures =
    organize io param.policy param.sig_only task.opens task.files in
  let collisions =
    if Fault.is_silent param.policy Codept_policies.module_conflict then
      Collisions.empty
    else
      Collisions.libs task units.mli in
  let collisions, file_set = Collisions.local collisions units.mli in
  let () =
    (* warns if any module is defined twice *)
    Collisions.handle param.policy collisions in
  let e = start_env io param task.libs signatures file_set in
  let {Unit.ml; mli} = solve param e units in
  let ml = remove_units task.invisibles ml in
  let mli = remove_units task.invisibles mli in
  {Unit.ml;mli}

(** Analysis step *)
let main_seed io param (task:Common.task) =
  let units, signatures =
    pre_organize io task.files in
  let file_set, gen = oracle param.policy
      (load_file io param.policy param.sig_only task.opens) units in
  let e = start_env io param task.libs signatures file_set in
  let units = solve_from_seeds task.seeds gen param e in
  let units = remove_units task.invisibles units in
  let units = List.fold_left (fun (pair: _ Unit.pair) (u:Unit.r)->
      match u.kind with
      | Structure -> { pair with ml = u :: pair.ml }
      | Signature -> {pair with mli = u :: pair.mli }
    ) { ml=[]; mli=[]} units in
  Unit.Groups.R.(split % group) units

let main io param (task:Common.task) =
  match task.seeds with
  | [] -> main_std io param task
  | _ -> main_seed io param task
