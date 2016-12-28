module M = Module
module S = Module.Sig
module Origin = M.Origin
module Def = Definition.Def

module type extended = sig
  include Interpreter.envt
  val resolve_alias: Paths.Simple.t -> t -> Name.t option
  val find_name: bool -> M.level -> Name.t -> t -> Module.t
  val restrict: t -> M.signature -> t
end

module type extended_with_deps =
sig
  type t
  include extended with type t:=t
  include Interpreter.with_deps with type t := t
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

module Base = struct
  type t = M.signature

  let empty = S.empty

  let proj lvl env = match lvl with
    | M.Module -> env.M.modules
    | M.Module_type -> env.module_types

  let find_name _root level name env =
    Name.Map.find name @@ proj level env

  let rec resolve_alias path env =
    match path with
    | [] -> None
    | a :: q ->
      match Name.Map.find a @@ proj Module env with
      | Alias {path; _ } -> Some (List.hd path)
      | M m -> resolve_alias q m.signature
    | exception Not_found -> None

  let is_exterior path envt =
    match path with
    | [] -> false (* should not happen *)
    | a :: _ ->
      match find_name () Module a envt with
      | M { origin = Unit _; _ } -> true
      | Alias _ -> false
      | exception Not_found -> true
      | _ -> false


  let rec find0 start_env require_root level path env =
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | a :: q ->
      match find_name false (adjust_level level q) a env with
      | M.Alias {path; _ } ->
        if require_root then
          raise Not_found
        else
          find0 start_env true level ( path @ q ) start_env
      | M.M m ->
        if require_root && not (is_unit m) then
          raise Not_found
        else if q = [] then
          m
        else
          find0 start_env false level q m.signature

  let find level path env=
    find0 env false level path env

  let deps _env = Paths.P.Set.empty
  let reset_deps = ignore

  let (>>) env def = Def.(env +@ def.Definition.visible)
  let restrict _env sg = sg
  let add_module = S.add
end

module Open_world(Envt:extended_with_deps) = struct
  module P = Paths.Pkg
  type t = { core: Envt.t; world: P.t Name.map; externs: P.set ref }

  let is_exterior path env = Envt.is_exterior path env.core

  let resolve_alias name env =
    Envt.resolve_alias name env.core

  let start core world = { core; world; externs = ref P.Set.empty }

  let last l = List.hd @@ List.rev l

  let reset_deps env =
    env.externs := P.Set.empty;
    Envt.reset_deps env.core

  let deps env =
    Paths.Pkg.Set.union !(env.externs) (Envt.deps env.core)

  let approx path =
    match path with
    | [] -> raise (Invalid_argument "Empty path")
    | [name] ->
      Module.create name
                ~origin:(Unit {Paths.P.source=Unknown; file=[name]})
                ~precision:Unknown
                Module.Sig.empty
    | l ->
      let name = last l in
      Module.create name ~origin:Submodule ~precision:Unknown S.empty

 let find_name root level name env =
    try Envt.find_name root level name env.core with
    | Not_found ->
      if root && Name.Map.mem name env.world then
        raise Not_found
      else
        Module.M ( approx [name] )

 let find level path env =
   (*   Format.printf "Open world looking for %a\n" Paths.S.pp path; *)
    try Envt.find level path env.core with
    | Not_found ->
      let aliased, root = match resolve_alias path env with
        | Some root -> true, root
        | None -> false, List.hd path in
      (* Format.printf "aliased:%b, true name:%s\n" aliased root; *)
      let undefined =
        aliased ||
        match Envt.find level [root] env.core with
        | exception Not_found -> true
        | _ -> false in
      if Name.Map.mem root env.world
      && record_level level path (* module types never come from files *)
      && undefined (* is root defined to be something else than
                           the unit root? *)
      then
        raise Not_found
      else if undefined then
        (
          if record_level level path then
            begin
              env.externs := P.Set.add
                  { P.file = [root]; source = Unknown } !(env.externs)
            end;
          approx path
        )
      else
        approx path

  let (>>) env sg = { env with core = Envt.( env.core >> sg ) }
  let add_module env m = { env with core = Envt.add_module env.core m }
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
    let cmis =
      Array.fold_left (fun m x ->
          if Filename.check_suffix x ".cmi" then
            let p = {P.source = P.Pkg origin; file = Paths.S.parse_filename x} in
            Name.Map.add (P.module_name p) p m
          else m
        )
        Name.Map.empty files in
    { origin; resolved= Envt.start Base.empty cmis; cmis }

  type t = { local: Base.t; local_units: Name.set; pkgs: source list }


  let is_exterior path env = Base.is_exterior path env.local
  let resolve_alias name env = Base.resolve_alias name env.local

  let create includes units env =
    { local = env; local_units = units; pkgs = List.map read_dir includes }

  module I = Interpreter.Make(Envt)(struct
      let polycy = Fault.Polycy.default
      let transparent_aliases = false
      (* we are not recording anything *)
      let transparent_extension_nodes = false
      (* extension nodes should not appear in cmi *)
    end)


  let rec track source stack = match stack with
    | [] -> ()
    | (name, path, code) :: q ->
      match I.m2l source.resolved code with
      | Error code ->
        begin match M2l.Block.m2l code with
          | None -> assert false
          | Some name' ->
            let path' = Name.Map.find name' source.cmis in
            let code' = Cmi.m2l @@ P.filename path' in
            track source ( (name', path', code') :: (name, path, code) :: q )
        end
      | Ok (_, sg) ->

        let md = M.create
            ~origin:(Origin.Unit path) name sg in
        source.resolved <- Envt.add_module source.resolved (M.M md);
        track source q

  let rec pkg_find name source =
    match Envt.find_name true M.Module name source.resolved with
    | M.M { origin = Unit { source = Unknown; _ }; _ } ->
      raise Not_found
    | exception Not_found ->
      let path = Name.Map.find name source.cmis in
      track source
        [name, path, Cmi.m2l @@ P.filename path ];
      pkg_find name source
    | m -> m

  let rec pkgs_find name = function
    | [] -> raise Not_found
    | source :: q ->
      try
        let m = pkg_find name source in
        m
      with Not_found ->
        pkgs_find name q

  let find_name root level name env =
    try Base.find_name root level name env.local with
    | Not_found when level = Module ->
      if root && Name.Set.mem name env.local_units then
        raise Not_found
      else
        pkgs_find name env.pkgs

  let restrict env sg = { env with local = Base.restrict env.local sg }

  let rec find0 start_env level path env =
    match path with
    | [] -> raise (Invalid_argument "Layered.find cannot find empty path")
    | a :: q  ->
      match find_name false (adjust_level level q) a env with
      | Alias {path; _ } -> find0 start_env level (path @ q ) start_env
      | M.M m ->
        if q = [] then m
        else
          find0 start_env level q (restrict env m.signature)

  let find level path env =
    if Name.Set.mem (List.hd path (* paths are not empty *)) env.local_units then
      Base.find level path env.local
    else
      find0 env level path env


  let (>>) e1 sg = { e1 with local = Base.( e1.local >> sg) }
  let add_module e m = { e with local = Base.add_module e.local m }

end


module Tracing(Envt:extended) = struct

  module P = Paths.Pkg
  type t = { env: Envt.t;
             deps: P.set ref
           }


  let is_exterior path env = Envt.is_exterior path env.env
  let resolve_alias name env = Envt.resolve_alias name env.env


  let path_record p env =
    env.deps := P.Set.add p !(env.deps)

  let extend env =
    { env; deps = ref P.Set.empty }


  let record env m =
    let open Module in
    match m.origin with
    | Origin.Unit p ->
      (* Format.printf "Recording %a\n" pp (Module.M m) ; *)
      path_record p env
    | _ -> ()

  let rec find0 start_env ~top ~require_root level path env =
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | a :: q ->
      match Envt.find_name top (adjust_level level q) a env.env with
      | Alias {path; _ } ->
        if require_root then
          (* we were looking for a compilation unit and got a submodule alias *)
          raise Not_found
        else
          (* aliases link only to compilation units *)
          find0 start_env ~top:true ~require_root:true
            level (path @ q) start_env
      | M.M m ->
        if require_root && not (is_unit m) then
          raise Not_found
        else begin
          record env m;
          if q = [] then
            m
          else
            find0 start_env ~top:false ~require_root:false level q
              { env with env = Envt.restrict env.env m.signature }
        end

  let find level path env =
    find0 env ~top:true ~require_root:false level path env

  let find_name root level name env =
    M.M (find0 env ~top:root ~require_root:false level [name] env)


  let (>>) e1 sg = { e1 with env = Envt.( e1.env >> sg) }

  let restrict env m = { env with env = Envt.restrict env.env m }
  let add_module e m = { e with env = Envt.add_module e.env m }

  let deps env = !(env.deps)
  let reset_deps env =  env.deps := Paths.P.Set.empty

end

module Trl = Tracing(Layered)
module Tr = Open_world(Trl)

module Interpreters = struct
  module Sg = Interpreter.Make(Base)
  module Tr = Interpreter.Make(Tr)
end
