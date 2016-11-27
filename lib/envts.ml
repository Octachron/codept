module M = Module
module S = Module.Sig
module Origin = M.Origin
module Def = Definition.Def

module type extended = sig
  include Interpreter.envt
  val find_name: bool -> M.level -> Name.t -> t -> Module.t
  val restrict: t -> M.signature -> t
end

module type extended_with_deps =
sig
  type t
  include extended with type t:=t
  include Interpreter.with_deps with type t := t
end

module Base = struct
  type t = M.signature

  let empty = S.empty

  let proj lvl env = match lvl with
    | M.Module -> env.M.modules
    | M.Module_type -> env.module_types

  let find_name _root level name env =
    Name.Map.find name @@ proj level env

  let rec find ~transparent ?alias level path env =
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | [a] -> find_name false level a env
    | a :: q ->
      let m = find_name false M.Module a env in
      find ~transparent ?alias level q m.signature

  let deps _env = Paths.P.Set.empty
  let reset_deps = ignore

  let (>>) = Def.(+@)
  let restrict _env sg = sg
  let add_module = S.add
end

module Open_world(Envt:extended_with_deps) = struct
  module P = Paths.Pkg
  type t = { core: Envt.t; world: P.t Name.map; externs: P.set ref }

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
      Module.(create name
                ~origin:(Unit {Paths.P.source=Unknown; file=[name]})
                Sig.empty)
    | l ->
      let name = last l in
      Module.(create name ~origin:Extern Sig.empty)

 let find_name root level name env =
    try Envt.find_name root level name env.core with
    | Not_found ->
      if root && Name.Map.mem name env.world then
        raise Not_found
      else
        approx [name]

 let record_level level = function
   | _ :: _ :: _ -> true
   | [_] -> level = M.Module
   | [] -> false

  let find ~transparent ?(alias=false) level path env =
    try Envt.find ~transparent ~alias level path env.core with
    | Not_found ->
      let root = List.hd path in
      let undefined root =
        match Envt.find ~transparent:true ~alias:true level [root] env.core with
        | exception Not_found -> true
        | _ -> false in
      if Name.Map.mem root env.world
      && record_level level path (* module types never come from files *)
      && undefined root (* is root defined to be something else than
                           the unit root? *)
      then
        raise Not_found
      else if not (alias && transparent) && undefined root then
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

  let create includes units env =
    { local = env; local_units = units; pkgs = List.map read_dir includes }

  module I = Interpreter.Make(Envt)(struct
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
            let path' = Name.Map.find name source.cmis in
            let code' = Cmi.m2l @@ P.filename path in
            track source ( (name', path', code') :: (name, path, code) :: q )
        end
      | Ok (_, sg) ->

        let md = M.create
            ~origin:(Origin.Unit path) name sg in
        source.resolved <- Envt.add_module source.resolved md;
        track source q

  let rec pkg_find name source =
    match Envt.find_name true M.Module name source.resolved with
    | { origin = Extern; _ }
    | { origin = Unit { source = Unknown; _ }; _ } ->
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

  let rec find0 level path env =
    match path with
    | [] -> raise (Invalid_argument "Layered.find cannot find empty path")
    | [a] ->
        find_name false level a env
    | a :: q ->
      let m = find_name false M.Module a env in
      find0 level q (restrict env m.signature)

  let find ~transparent ?alias level path env =
    if Name.Set.mem (List.hd path (* paths are not empty *)) env.local_units then
      Base.find ~transparent ?alias level path env.local
    else
      find0 level path env


  let (>>) e1 sg = { e1 with local = Base.( e1.local >> sg) }
  let add_module e m = { e with local = Base.add_module e.local m }

end


module Tracing(Envt:extended) = struct

  module P = Paths.Pkg
  type t = { env: Envt.t;
             deps: P.set ref
           }

  let record p env =
    env.deps := P.Set.add p !(env.deps)

  let extend env =
    { env; deps = ref P.Set.empty }

  let record_or_delay alias env m =
    let open Module in
    match m.origin with
    | Origin.Unit p -> if not alias then
        (record p env; None)
      else
        Some p
    | Alias (Unit p) ->
      if not alias then
        (record p env; None)
      else
        Some p
    | _ -> None

  let delayed_record env = Option.iter (fun p ->
      record p env
    )

  let rec find0 ~transparent start_env start delayed level path env =
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | [a] ->
      delayed_record start_env delayed;
      let m = Envt.find_name start level a env.env in
      if start && level = Module then
        ignore @@ record_or_delay transparent env m;
      if not transparent then
        begin
          match m.origin with
          | Alias (Unit p) ->
            if not (level = Module_type && start) then
                record p start_env
          | _ -> ()
        end;
      m
    | a :: q ->
      delayed_record start_env delayed;
      let m = Envt.find_name start M.Module a env.env in
      let delayed =
        if start then
          record_or_delay transparent env m
        else if transparent then
          match delayed, m.origin with
          | Some _, Alias (Unit w) -> Some w
          | _ -> None
        else
          match m.origin with
          | Alias (Unit p) -> record p start_env; None
          | _ -> None
      in
      find0 ~transparent start_env false delayed level q
        { env with env = Envt.restrict env.env m.signature}

  let find ~transparent ?(alias=false) level path env =
    find0 ~transparent:(transparent && alias) env true None level path env

  let find_name root level name env =
    find0 ~transparent:true env root None level [name] env


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
