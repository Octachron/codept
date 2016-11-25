module M = Module
module S = Module.Sig
module Origin = M.Origin
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
  module P = Paths.Pkg
  type t = { core: Envt.t; world: P.t Name.map; externs: P.set ref }

  let start core world = { core; world; externs = ref P.Set.empty }

  let last l = List.hd @@ List.rev l

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

 let find_name level name env =
    try Envt.find_name level name env.core with
    | Not_found ->
      if Name.Map.mem name env.world then
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
      if Name.Map.mem root env.world then
        raise Not_found
      else if not (alias && transparent) && undefined root then
        (
          if record_level level path then
            env.externs := P.Set.add
                { P.file = [root]; source = Unknown } !(env.externs);
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
    match Envt.find_name M.Module name source.resolved with
    | { origin = Extern; _ } -> raise Not_found
    | exception Not_found ->
      let path = Name.Map.find name source.cmis in
      track source
        [name, path, Cmi.m2l @@ P.filename path ];
      pkg_find name source
    | m -> m

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

  module P = Paths.Pkg
  type t = { env: Envt.t;
             deps: P.set ref
           }

  let rec resolve n env =
    match (Envt.find_name Module n env).M.origin with
    | Origin.Unit p -> p
    | Origin.Alias n -> resolve n env
    | Origin.Extern -> { source = Unknown; file = [n] }
    | exception Not_found  -> { source = Unknown; file = [n] }
    | Arg | Rec | First_class | Submodule ->
      assert false

  let record n env =
    env.deps := P.Set.add (resolve n env.env) !(env.deps)


  let rec record_alias_origin n env =
    match (Envt.find_name Module n env.env).M.origin with
    | Origin.Unit _ -> record n env
    | Origin.Alias n -> record_alias_origin n env
    | exception Not_found  -> record n env
    (* if an aliased module is not found, then it is an external module which
       dependency should be recorded. *)
    | Arg | Rec | First_class | Submodule | Extern -> ()

  let extend env =
    { env; deps = ref P.Set.empty }

  let smart_record alias env m =
    let open Module in
    match m.origin with
    | Origin.Unit _ -> record m.name env; None
    | Alias n ->
      if not alias then
        (record n env; None)
      else
        Some n
    | _ -> None

  let delayed_record env = Option.iter (fun n ->
      record n env
    )

  let rec find0 ~transparent start delayed level path env =
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | [a] ->
      delayed_record env delayed;
      let m = Envt.find_name level a env.env in
      if start && level = Module then
        ignore @@ smart_record transparent env m;
      if not transparent then
        begin
          match m.origin with
          | Alias n -> record_alias_origin n env
          | _ -> ()
        end;
      m
    | a :: q ->
      delayed_record env delayed;
      let m = Envt.find_name M.Module a env.env in
      let delayed =
        if start then
          smart_record transparent env m
        else if transparent then
          match delayed, m.origin with
          | Some _, Alias w -> Some w
          | _ -> None
        else
          match m.origin with
          | Alias n -> record_alias_origin n env; None
          | _ -> None
      in
      find0 ~transparent false delayed level q
        { env with env = Envt.restrict env.env m.signature}

  let find ~transparent ?(alias=false) level path env =
    find0 ~transparent:(transparent && alias) true None level path env

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
