module M = Module
module S = Module.Sig
module Edge = Deps.Edge
module Origin = M.Origin
module Y = Summary
module Nms = Namespaced
module Out = Outliner

type answer = Out.answer =
  | M of Module.m
  | Namespace of { name:Name.t; modules:Module.dict }

type context =
  | Signature of M.signature
  | In_namespace of M.dict

module Query = struct

  type 'a t = 'a Outliner.query_result
  let pure main = { Outliner.main; msgs = [] }
  let (++) (query:_ t) fault = { query with msgs = fault :: query.msgs }
  let create main msgs : _ t = {main; msgs}
  let fmap f (q: _ t) : _ t = { q with main = f q.main }
  let (>>=) (x: _ t) f: _ t =
    let {main;msgs}: _ t = f x.main in
    { main; msgs = msgs @ x.msgs }
  let add_msg msgs (q: _ t) = { q with msgs = msgs @ q.msgs }

end


module type extended = sig
  include Outliner.envt
  val top: t -> t
  val find_name: ?edge:Edge.t -> root:bool -> M.level -> Name.t -> t
    -> Module.t Query.t
  val restrict: t -> context -> t
end

module type extended_with_deps =
sig
  type t
  include extended with type t:=t
  include Outliner.with_deps with type t := t
end

(* compute if the level of the root of the path is
   at level module
 *)
let record_level level = function
  | _ :: _ :: _ -> true
  | [_] -> level = M.Module
  | [] -> false

let adjust_level level = function
  | [] -> level
  | _ :: _ -> M.Module

let is_unit = function
  |{ M.origin = Origin.Unit _ ; _ } -> true
  | _ -> false

let ambiguity name breakpoint =
  let f = Standard_faults.ambiguous in
  { f  with
    Fault.log = (fun lvl l -> f.log lvl l name breakpoint)
  }

module Base = struct
  type t = { top: M.dict; current: context }
  let eq x y = x.top = y.top
  let empty = { top = Name.Map.empty; current = In_namespace M.empty }
  let start s =
    { top = s; current = Signature (Exact (M.Def.modules s) )}

  let top env = { env with current = Signature (Exact (M.Def.modules env.top) ) }

  let proj lvl def = match lvl with
    | M.Module -> def.M.modules
    | M.Module_type -> def.module_types

  let rec find_name level name current =
    match current with
    | Signature Module.Blank ->
      (* If we have no information on the current signature,
         we create a mockup module for the requested submodule *)
      Query.pure @@ M.md @@ Module.mockup name
    | Signature Exact def ->
      Query.pure @@ Name.Map.find name @@ proj level def
    | Signature Divergence d ->
      (* If we have a divergent signature, we first look
         at the signature after the divergence: *)
      begin match Name.Map.find_opt name @@ proj level d.after with
      | Some m -> Query.pure m
      | None ->
        (* We then try to find the searched name in the signature
           before the divergence *)
        let open Query in
        ( find_name level name (Signature d.before) >>= fun q ->
        (* If we found the expected name before the divergence,
           we add a new message to the message stack, and return
           the found module, after marking it as a phantom module. *)
          create (Module.spirit_away d.point q) [ambiguity name d.point] )
      end
    | In_namespace modules ->
      try
        Query.pure @@ Name.Map.find name modules
      with Not_found -> raise Not_found
  let find_name ?edge:_ ~root:_ level name env = find_name level name env.current

  let restrict env context = { env with current = context }

  let rec resolve_alias_md path def =
    match path with
    | [] -> None
    | a :: q ->
      match Name.Map.find a def with
      | M.Alias {path; weak = false; _ } -> Some path
      | M.Alias { weak = true; _ } -> None
      | M m -> resolve_alias_sign q m.signature
      | Namespace n -> resolve_alias_md q n.modules
      | exception Not_found -> None
  and resolve_alias_sign path = function
    | Blank -> None
    | Exact s -> resolve_alias_md path s.modules
    | Divergence d ->
      match resolve_alias_md path d.after.modules with
      | Some _ as r -> r
      | None ->
        (* FIXME: Should we warn here? *)
        resolve_alias_sign path d.before

  let resolve_alias path env =
    match env.current with
    | In_namespace md -> resolve_alias_md path md
    | Signature sg -> resolve_alias_sign path sg

  let is_exterior path envt =
    match path with
    | [] -> false (* should not happen *)
    | a :: _ ->
      match (find_name () Module a envt).main with
      | M { origin = Unit _; _ } -> true
      | Alias _ -> false
      | exception Not_found -> true
      | _ -> false


  let rec find0 ?edge require_root level path env =
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | a :: q ->
      let open Query in
      find_name ?edge ~root:false (adjust_level level q) a env >>=
      function
      | M.Alias { weak = true; _ } -> raise Not_found
      | M.Alias {path; weak = false; _ } ->
        if require_root then
          raise Not_found
        else
          find0 true level ( Namespaced.flatten path @ q ) (top env)
      | M.M m ->
        if require_root && not (is_unit m) then
          raise Not_found
        else if q = [] then
          pure (Out.M m)
        else
          find0 false level q  @@ restrict env (Signature m.signature)
      | Namespace {name;modules}  ->
        if q = [] then pure (Namespace {name;modules}) else
          find0 false level q @@ restrict env (In_namespace modules)

  let find ?(edge=Edge.Normal) level path env=
    find0 ~edge false level path env

  let deps _env = Paths.P.Map.empty
  let reset_deps = ignore

  let to_sign = function
    | Signature s -> s
    | In_namespace modules ->
      M.Exact { M.Def.empty with modules }

  let (>>) env def =
    restrict env @@
    Signature (Y.extend (to_sign env.current) (Y.strenghen def))

  let add_unit env ?(namespace=[]) x =
    let m: Module.t = M.with_namespace namespace x in
    let top = Name.Map.add  (M.name m)  m env.top in
    start top

  let add_namespace env (nms:Namespaced.t) =
    if nms.namespace = [] then env else
    let top = M.Dict.( union env.top @@ of_list [Module.namespace nms] ) in
    start top

end


module Open_world(Envt:extended_with_deps) = struct

  let fault path =
    let f = Standard_faults.unknown_approximated in
    { f with Fault.log = (fun lvl -> f.log lvl path) }


  module P = Paths.Pkg
  type t = { core: Envt.t; world: Name.Set.t;
             externs: Edge.t P.map ref }

  let eq x y = Envt.eq x.core y.core
  let is_exterior path env = Envt.is_exterior path env.core
  let top env = { env with core = Envt.top env.core }

  let resolve_alias name env =
    Envt.resolve_alias name env.core

  let start core world = { core; world; externs = ref P.Map.empty }

  let last l = List.hd @@ List.rev l

  let reset_deps env =
    env.externs := P.Map.empty;
    Envt.reset_deps env.core

  let deps env =
    Deps.merge !(env.externs) (Envt.deps env.core)

  let approx path =
    match path with
    | [] -> raise (Invalid_argument "Empty path")
    | [name] ->
      Module.mockup name ~path:{Paths.P.source=Unknown; file=[name]}
    | l -> let name = last l in Module.mockup name

 let find_name ?edge ~root level name env =
    try Envt.find_name ?edge ~root level name env.core with
    | Not_found ->
      if root && Name.Set.mem name
           env.world then
        raise Not_found
      else
        Query.pure @@ M.M (approx [name])

 let find ?(edge=Edge.Normal) level path env =
   (*   Format.printf "Open world looking for %a\n" Paths.S.pp path; *)
    try Envt.find ~edge level path env.core with
    | Not_found ->
      let aliased, root = match resolve_alias path env with
        | Some root -> true, root
        | None -> false, Namespaced.make @@ List.hd path in
      (* Format.printf "aliased:%b, true name:%s\n" aliased root; *)
      let undefined =
        aliased ||
        match Envt.find Module (Nms.flatten root) env.core with
        | exception Not_found -> true
        | _ -> false in
      if Name.Set.mem (Namespaced.head root) env.world
      (* module types never come from files *)
      && record_level level path
      (* is root defined to be something else than the unit root? *)
      && undefined
      then
        raise Not_found
      else if undefined then
        (
          if record_level level path then
            begin
              env.externs := Deps.update
                  { P.file = Nms.flatten root;
                    source = Unknown } edge !(env.externs)
            end;
          Query.( pure (M(approx path)) ++ fault path )
        )
      else
        Query.create (M(approx path)) [fault path]

  let (>>) env sg = { env with core = Envt.( env.core >> sg ) }
  let add_unit env ?namespace m =
    { env with core = Envt.add_unit env.core ?namespace m }
  let add_namespace env n = { env with core = Envt.add_namespace env.core n }
  let restrict env m = { env with core = Envt.restrict env.core m }

end

module Layered = struct
  module P = Paths.Pkg
  module Envt = Open_world(Base)
  type source = {
    origin: Paths.Simple.t;
    mutable resolved: Envt.t;
    cmis: P.t Name.map
  }


  let read_dir dir =
    let files = Sys.readdir dir in
    let origin = Paths.S.parse_filename dir in
    let cmis_set, cmis_map =
      Array.fold_left (fun (s,m) x ->
          if Filename.check_suffix x ".cmi" then
            let p = {P.source = P.Pkg origin; file = Paths.S.parse_filename x} in
            Name.Set.add (P.module_name p) s,
            Name.Map.add (P.module_name p) p m
          else s, m
        )
        (Name.Set.empty, Name.Map.empty) files in
    { origin; resolved= Envt.start Base.empty cmis_set; cmis= cmis_map }

  type t = { local: Base.t; local_units: Name.Set.t; pkgs: source list }

  let eq x y = Base.eq x.local y.local
  let is_exterior path env = Base.is_exterior path env.local
  let resolve_alias name env = Base.resolve_alias name env.local
  let top env = { env with local = Base.top env.local }


  let create includes units env =
    { local = env; local_units = units; pkgs = List.map read_dir includes }

  module I = Outliner.Make(Envt)(struct
      let policy = Standard_policies.quiet
      let transparent_aliases = false
      (* we are not recording anything *)
      let transparent_extension_nodes = false
      (* extension nodes should not appear in cmi *)
      let epsilon_dependencies = false
      (* do no try epsilon dependencies yet *)
    end)


  let rec track source stack = match stack with
    | [] -> ()
    | (name, path, code) :: q ->
      match I.m2l path source.resolved code with
      | Error code ->
        begin match M2l.Block.m2l code with
          | None -> assert false
          | Some { data = _y, bl_path ; _ } ->
            let name' = List.hd bl_path in
            let path' = Name.Map.find name' source.cmis in
            let code' = Cmi.m2l @@ P.filename path' in
            track source ( (name', path', code') :: (name, path, code) :: q )
        end
      | Ok (_, sg) ->
        let md = M.create
            ~origin:(Origin.Unit path) name sg in
        source.resolved <- Envt.add_unit source.resolved (M.M md);
        track source q

  let rec pkg_find name source =
    match Envt.find_name true M.Module name source.resolved with
    | {main = M.M { origin = Unit { source = Unknown; _ }; _ }; _ } ->
      raise Not_found
    | exception Not_found ->
      let path = Name.Map.find name source.cmis in
      track source
        [name, path, Cmi.m2l @@ P.filename path ];
      pkg_find name source
    | m -> m.main

  let rec pkgs_find name = function
    | [] -> raise Not_found
    | source :: q ->
      try
        let m = pkg_find name source in
        m
      with Not_found ->
        pkgs_find name q

  let find_name ?edge ~root level name env =
    try Base.find_name ?edge ~root level name env.local with
    | Not_found when level = Module ->
      if root && Name.Set.mem name env.local_units then
        raise Not_found
      else
        Query.pure @@ pkgs_find name env.pkgs

  let restrict env sg = { env with local = Base.restrict env.local sg }

  let rec find0 level path env =
    match path with
    | [] -> raise (Invalid_argument "Layered.find cannot find empty path")
    | a :: q  ->
      let open Query in
      find_name false (adjust_level level q) a env >>=
      function
      | Alias { weak = true; _ } -> raise Not_found
      | Alias {path; weak= false; _ } ->
        find0 level (Namespaced.flatten path @ q ) (top env)
      | M.M m ->
        if q = [] then pure (M m)
        else
          find0 level q (restrict env @@ Signature m.signature)
      | Namespace {name;modules} ->
        if q = [] then pure (Namespace {name;modules})
        else find0 level q (restrict env @@ In_namespace modules)

  let find ?edge:_ level path env =
    if Name.Set.mem (List.hd path (* paths are not empty *)) env.local_units then
      Base.find level path env.local
    else
      find0 level path env


  let (>>) e1 sg = { e1 with local = Base.( e1.local >> sg) }
  let add_unit e ?namespace m =
    { e with local = Base.add_unit e.local ?namespace m }
  let add_namespace env n =
    { env with local = Base.add_namespace env.local n }

end


module Tracing(Envt:extended) = struct

  module P = Paths.Pkg
  type t = { env: Envt.t;
             deps: Edge.t P.Map.t ref
           }


  let eq x y= Envt.eq x.env y.env
  let is_exterior path env = Envt.is_exterior path env.env
  let resolve_alias name env = Envt.resolve_alias name env.env
  let top env = { env with env = Envt.top env.env }
  let restrict env m = { env with env = Envt.restrict env.env m }



  let path_record edge p env =
    env.deps := Deps.update p edge !(env.deps)

  let extend env =
    { env; deps = ref P.Map.empty }

  let phantom_record name env =
    path_record Edge.Normal { P.source = Unknown; file = [name] } env

  let record edge root env (m:Module.m) =
    match m.origin with
    | Origin.Unit p ->
      path_record edge p env; []
    | Phantom (phantom_root, b) ->
      if root && not phantom_root then
        (phantom_record m.name env; [ambiguity m.name b] ) else []
    | _ -> []

  let rec find0 ?(edge=Edge.Normal) ~root ~require_top level path env =
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | a :: q ->
      let open Query in
      Envt.find_name root (adjust_level level q) a env.env >>=
      function
      | Alias { weak = true; _ } -> raise Not_found
      | Alias {path; phantom; name; weak= false } ->
        let msgs =
          match phantom with
          | None -> []
          | Some b ->
            if root then
            (phantom_record name env; [ambiguity name b])
          else [] in
        if require_top then
          (* we were looking for a compilation unit and got a submodule alias *)
          raise Not_found
        else
          (* aliases link only to compilation units *)
          Query.add_msg msgs @@
          find0 ~root:true ~require_top:true
            level (Namespaced.flatten path @ q) (top env)
      | M.M m ->
        if require_top && not (is_unit m) then
          raise Not_found
        else begin
          let faults = record edge root env m in
          if q = [] then
            create (M m) faults
          else
            find0 ~root:false ~require_top:false level q
              @@ restrict env @@ Signature m.signature
        end
      | Namespace {name;modules} ->
        begin
          (*          let faults = record edge root env m in *)
          (* FIXME: record Namespace deps *)
          if q = [] then
            pure (Namespace {name;modules})
          else
            find0 ~root:false ~require_top:false level q
              @@ restrict env @@ In_namespace modules

        end

  let find ?edge level path env =
    find0 ?edge ~root:true ~require_top:false level path env

  let find_name ?(edge=Edge.Normal) ~root level name env =
    Query.fmap (function Namespace {name;modules} -> M.Namespace {name;modules}
                   | M x -> M.M x )
    (find0 ~edge ~root ~require_top:false level [name] env)


  let (>>) e1 sg = { e1 with env = Envt.( e1.env >> sg) }


  let add_unit e ?namespace m = { e with env = Envt.add_unit e.env ?namespace m }
  let add_namespace env n = { env with env = Envt.add_namespace env.env n }

  let deps env = !(env.deps)
  let reset_deps env =  env.deps := Paths.P.Map.empty

end

module Trl = Tracing(Layered)
module Tr = Open_world(Trl)
