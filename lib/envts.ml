module M = Module
module S = Module.Sig
module Def = Definition.Def

module type extended = sig
  include Interpreter.envt
  val find_name: M.level -> Name.t -> t -> Module.t
  val restrict: t -> M.signature -> t
end

module Base = struct
  type t = M.signature

  let empty = S.empty

  let proj lvl env = match lvl with
    | M.Module -> env.M.modules
    | M.Module_type -> env.module_types

  let root_origin level path env =
    let a, lvl = match path with
      | []-> raise @@ Invalid_argument "Compute.Envt.root_origin: empty path"
      | [a] -> a, level
      | a :: _ -> a, M.Module in
    let m = Name.Map.find a @@ proj lvl env in
    m.origin

  let find_name level name env =
    Name.Map.find name @@ proj level env

  let rec find ~transparent ?alias level path env =
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | [a] -> find_name level a env
    | a :: q ->
      let m = find_name M.Module a env in
      find ~transparent ?alias level q m.signature

  let (>>) = Def.(+@)
  let restrict _env sg = sg
  let add_module = S.add
end

module Open_world(Envt:extended) = struct
  module P = Package.Path
  type t = { core: Envt.t; world: P.t Name.map; mutable externs: Name.set }

  let start core world = { core; world; externs = Name.Set.empty }

  let approx name =
    Module.(create name ~origin:Extern Sig.empty)

 let find_name level name env =
    try Envt.find_name level name env.core with
    | Not_found ->
      if Name.Map.mem name env.world then
        raise Not_found
      else
        approx name

  let last l = List.hd @@ List.rev l

  let find ~transparent ?alias level path env =
    try Envt.find ~transparent ?alias level path env.core with
    | Not_found ->
      let root = List.hd path in
      if Name.Map.mem root env.world then
        raise Not_found
      else
        (
          env.externs <- Name.Set.add root env.externs;
          approx (last path)
        )

  let (>>) env sg = { env with core = Envt.( env.core >> sg ) }
  let add_module env m = { env with core = Envt.add_module env.core m }
  let restrict env m = { env with core = Envt.restrict env.core m }

end

module Sg = Interpreter.Make(Base)

module Layered = struct
  module P = Package.Path
  module Envt = Open_world(Base)
  type source = {
    origin:Npath.t;
    mutable resolved: Envt.t;
    cmis: P.t Name.map
  }

  let read_dir dir =
    let files = Sys.readdir dir in
    let origin = P.parse_filename dir in
    let cmis =
      Array.fold_left (fun m x ->
          if Filename.check_suffix x ".cmi" then
            let p = {P.package = P.Sys origin; file = P.parse_filename x} in
            Name.Map.add (P.module_name p) p m
          else m
        )
        Name.Map.empty files in
    { origin; resolved= Envt.start Base.empty cmis; cmis }

  type t = { local: Base.t; pkgs: source list }

  let create includes env = { local = env; pkgs = List.map read_dir includes }

  module I = Interpreter.Make(Envt)(struct
      let transparent_aliases = false
      (* we are not recording anything *)
      let transparent_extension_nodes = false
      (* extension nodes should not appear in cmi *)
    end)


  let rec track source stack = match stack with
    | [] -> ()
    | (name, code) :: q ->
      match I.m2l source.resolved code with
      | Halted code ->
        begin match M2l.Block.m2l code with
          | None -> assert false
          | Some name' ->
            let code' = Cmi.cmi_m2l
              @@ P.filename
              @@ Name.Map.find name source.cmis in
            track source ( (name',code') :: (name, code) :: q )
        end
      | Done (_, sg) ->
        let md = M.create ~origin:(M.Unit (Pkg source.origin)) name sg in
        source.resolved <- Envt.add_module source.resolved md;
        track source q

  let rec pkg_find name source =
    try
      let m = Envt.find_name M.Module name source.resolved
      in m
    with
    | Not_found ->
      track source
        [name,Cmi.cmi_m2l (P.filename @@ Name.Map.find name source.cmis)];
      pkg_find name source

  let rec pkgs_find name = function
    | [] -> raise Not_found
    | source :: q ->
      try pkg_find name source with Not_found -> pkgs_find name q

  let find_name level name env =
    try Base.find_name level name env.local with
    | Not_found when level = Module -> pkgs_find name env.pkgs

  let restrict env sg = { env with local = Base.restrict env.local sg }

  let rec find ~transparent ?alias level path env =
    match path with
    | [] -> raise (Invalid_argument "Layered.find cannot find empty path")
    | [a] -> find_name level a env
    | a :: q ->
      let m = find_name M.Module a env in
      find ~transparent ?alias level q (restrict env m.signature)


  let (>>) e1 sg = { e1 with local = Base.( e1.local >> sg) }
  let add_module e m = { e with local = Base.add_module e.local m }

end


module Tracing(Envt:extended) = struct

  module P = Package.Path
  type t = { env: Envt.t;
             deps: P.set ref;
             cmi_deps: P.set ref
           }

  let resolve n env =
    let package =
      match (Envt.find_name Module n env).M.origin with
      | M.Unit Local | M.Alias _ -> P.Local
      | M.Extern -> Unknown
      | M.Unit (Pkg p) -> Sys p
      | M.Arg | M.Rec | M.First_class | Submodule ->
        assert false
      | exception Not_found -> Unknown
    in
    { P.package; file = [n] }

  let record n env =
    env.deps := P.Set.add (resolve n env.env) !(env.deps)
  let record_cmi n env = env.cmi_deps :=
      P.Set.add (resolve n env.env) !(env.cmi_deps)


  let extend env =
    { env; deps = ref P.Set.empty; cmi_deps = ref P.Set.empty }

  let deps env = env.deps

  let name path = List.hd @@ List.rev path

  let alias_chain start env a root = function
    | Module.Alias n when start-> Some n
    | Alias n -> Option.( root >>| fun r -> record_cmi r env; n )
    | Unit _ when start -> Some a
    | _ -> None

  let guard transparent f x = if transparent then f x else None

  let prefix level a env =
    match Envt.find_name level a env.env with
    | exception Not_found -> a
    | { origin = Alias n; _ } -> n
    | _ -> a

  let rec find0 ~transparent start root level path env =
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | [a] ->
      let m = Envt.find_name level a env.env in
      let root = guard transparent (alias_chain start env a root) m.origin in
      root, m
    | a :: q ->
      let m = Envt.find_name M.Module a env.env in
      let root = guard transparent (alias_chain start env a root) m.origin in
      find0 ~transparent false root level q
        { env with env = Envt.restrict env.env m.signature}

  let check_alias name env =
    match (Envt.find_name Module name env.env).origin with
    | Unit _ -> true
    | exception Not_found -> false
    | _ -> false

  let find ~transparent ?(alias=false) level path env =
    match find0 ~transparent true None level path env with
    | Some root, x when not transparent || not alias
      ->
      ( if level = Module && check_alias root env then record root env; x)
    | Some _, x -> x
    | None, x ->
      let root = prefix level (List.hd path) env in
      (if level = Module && check_alias root env then record root env; x)

  let find_name level name = find ~transparent:true ~alias:false level [name]

  let (>>) e1 sg = { e1 with env = Envt.( e1.env >> sg) }

  let restrict env m = { env with env = Envt.restrict env.env m }
  let add_module e m = { e with env = Envt.add_module e.env m }

end

module Trl = Tracing(Layered)
module Tr = Open_world(Trl)

module Interpreters = struct
  module Sg = Interpreter.Make(Base)
  module Tr = Interpreter.Make(Tr)
end
