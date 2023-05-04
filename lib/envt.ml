module M = Module
module Edge = Deps.Edge
module T = Transforms
module Y = Summary
open Debug

type answer_type = T.answer_type =
  | Namespace of Module.dict
  | Mty of Module.sty
type answer = T.answer = { name: Name.t; kind: answer_type }

type context =
  | Signature of M.signature
  | In_namespace of M.dict

module Query = struct

  type 'a t = 'a T.query_result option
  let return main = Some { T.main; deps = Deps.empty; msgs = [] }
  let deps deps = Some {T.main=(); deps; msgs=[]}

  let (>>|) x f = { x with T.main = f x.T.main }
  let (>>?) (x: _ t) f = let open Option in
    x >>= fun x -> f x.T.main >>| fun q ->
    { q with T.msgs= x.msgs @ q.T.msgs; deps=Deps.merge q.T.deps x.deps }

  let (>>) x y = x >>? fun () -> y

  let (<!>) x msgs =
    Option.fmap (fun x -> { x with T.msgs = msgs @ x.T.msgs }) x

end
open Query

type module_provider = Uloc.t -> Name.t -> Module.t Query.t
let last l = List.hd @@ List.rev l

let to_context s = Signature (Exact s)

let ambiguity loc name breakpoint =
  Fault.emit Standard_faults.ambiguous (loc,name,breakpoint)

let nosubmodule loc current level name =
  Fault.emit Standard_faults.nonexisting_submodule (loc,current,level,name)

let unknown loc mlvl path =
  Fault.emit Standard_faults.unknown_approximated (loc,mlvl,path)

let noloc = Uloc.none

module Core = struct

  type t = {
    top: M.Dict.t;
    current: context;
    providers: module_provider list;
  }

  let empty = {
    top = Name.Map.empty;
    current = Signature(Exact(M.Def.empty));
    providers = []
  }

  let eq x y= x.top = y.top
  let start s =
    { top = s.M.modules; current = to_context s;
      providers = []
    }

  let pp_context ppf = function
    | In_namespace modules ->
      Pp.fp ppf "namespace [%a]@." Module.pp_mdict modules
    | Signature sg -> Pp.fp ppf "[%a]@." Module.pp_signature sg

  let pp ppf x = Pp.fp ppf "@[top=%a@ context=%a@]"
      M.pp_mdict x.top pp_context x.current

  module D = struct
    let path_record ~path ?aliases ~edge pkg  =
      debug "Recording: %a" Namespaced.pp path;
      deps (Deps.make ~path ?aliases ~edge pkg )

    let phantom_record ?aliases name =
      path_record ~path:(Namespaced.make name) ?aliases ~edge:Edge.Normal
        { Pkg.source = Unknown; file = Namespaced.make name }

    let record loc edge ?aliases root name (m:Module.tracked_signature) =
      match m.origin with
      | M.Origin.Unit p -> path_record ~path:p.path ?aliases ~edge p.source
      | Phantom (phantom_root, b) when root && not phantom_root ->
        phantom_record ?aliases name <!> [ambiguity loc name b]
      | _ -> return ()
  end

  let request loc lvl name env =
    let rec request name  = function
      | [] -> None
      | f :: q ->
        match f loc name with
        | Some _ as q -> q
        | None -> request name q in
    if lvl = M.Module then
      request name env.providers
    else None


  let proj lvl def = match lvl with
    | M.Module -> def.M.modules
    | M.Module_type -> def.module_types


  let adjust_level level = function
    | [] -> level
    | _ :: _ -> M.Module

  let restrict env context = { env with current = context }
  let top env =
    { env with current = Signature (Exact (M.Def.modules env.top) ) }

  let find_opt name m =
    match Name.Map.find name m with
    | exception Not_found -> None
    | x -> return x

  let rec find_name loc phantom level name current =
    match current with
    | Signature Module.Blank ->
      (* we are already in error mode here, no need to emit yet another warning *)
      return (M.md @@ M.mockup name)
    | In_namespace modules ->
      if level = M.Module_type then None
      else find_opt name modules
    | Signature Exact def -> find_opt name @@ proj level def
    | Signature Divergence d ->
      (* If we have a divergent signature, we first look
         at the signature after the divergence: *)
      match find_opt name @@ proj level d.after with
      | Some _ as x -> x
      | None ->
        let open Query in
        let (|||) = Option.(|||) in

        (* We then try to find the searched name in the signature
           before the divergence *)
        begin find_name loc true level name (Signature d.before) >>? fun q ->
          let m = Module.spirit_away d.point q in
          if phantom then return m else return m <!> [ambiguity loc name d.point]
          (* If we found the expected name before the divergence,
              we add a new message to the message stack, and return
              the found module, after marking it as a phantom module. *)
        end
        (* If we did not find anything and were looking for a module type,
           we return a mockup module type *)
        ||| lazy
          (if level = Module_type then
             return (M.md @@ M.mockup name) <!> [unknown loc Module_type name]
           else None)

  let find_name loc = find_name loc false

  type ctx =
    | Any (** look for aliases too *)
    | Concrete (** we are looking for a concrete unit *)
    | Submodule

  let is_top = function Any | Concrete -> true | Submodule -> false

  type option =
    { loc: Uloc.t; level:M.level; edge:Edge.t; approx_submodule:bool }

  (** Should we return a mockup module and a warning? *)
  let approx_submodule o ctx lvl =
    o.approx_submodule && (ctx = Submodule || lvl=M.Module_type)

  let rec find option aliases ctx current env path =
    debug "looking for %a" Paths.S.pp path;
    debug "in %a, sub-approx: %B" pp_context env.current option.approx_submodule;
    match path with
    | [] -> None (* should not happen *)
    | a :: q ->
      let lvl = adjust_level option.level q in
      let r = match find_name option.loc lvl a env.current with
        | None when approx_submodule option ctx lvl ->
          debug "submodule approximate %s" (last path);
          return (M.md @@ M.mockup @@ last path)
          <!> [nosubmodule option.loc current lvl a]
        | None -> request option.loc lvl a env
        | Some _ as x -> x in
      r >>? find_elt option aliases ctx env (a::current) a q
  and find_elt option aliases ctx env current name q = function
    | Module.Alias {path; phantom } ->
      debug "alias to %a" Namespaced.pp path;
      let aliases = Namespaced.Set.add (Namespaced.of_path @@ List.rev current) aliases in
      let m = match phantom with
        | Some b when is_top ctx ->
          D.phantom_record name <!> [ambiguity option.loc name b]
        | None | Some _ -> return () in
      (* aliases link only to compilation units *)
      m >> find option aliases Any [] (top env) (Namespaced.flatten path @ q)
    | Module.Link _ when ctx = Concrete -> None
    | Module.Link path ->
      find option aliases Concrete [] (top env) (Namespaced.flatten path @ q)
    | M.Sig m ->
      debug "found module %s" name;
      D.record option.loc option.edge ~aliases (is_top ctx) name m >>
      if q = [] then return {name; kind = Mty (Sig m)}
      else
        find option aliases Submodule current (restrict env @@ Signature m.signature) q
    | Module.Abstract _ | Module.Fun _ as kind ->
      begin match q with
        | [] -> return {name; kind= Mty (Module.Partial.of_extended_mty kind)}
        | _ :: _  ->
          let name = last q in
          let mock = M.mockup name in
          let lvl = adjust_level option.level q in
          return {name; kind=Mty (Sig mock) } <!> [nosubmodule option.loc current lvl name]
      end
    | Module.Namespace modules ->
      (* let faults = record edge root env name in*)
      if q = [] then return {name; kind = Namespace modules}
      else find option aliases ctx current (restrict env @@ In_namespace modules) q

  let find loc sub ?edge level path envt =
    let edge = Option.default Edge.Normal edge in
    let option = {loc; approx_submodule=sub; edge; level } in
    match find option Namespaced.Set.empty Any [] envt path with
    | None -> raise Not_found
    | Some x -> x

  let find_implicit loc = find loc false
  let find loc = find loc true

  let find_within m loc ?edge level path envt =
    find loc ?edge level path @@ restrict envt (Signature m)

  let to_sign = function
    | Signature s -> s
    | In_namespace modules -> M.Exact { M.Def.empty with modules }

  let extend env def =
    restrict env @@ Signature (Y.extend (to_sign env.current) def)

  let add_unit env ?(namespace=[]) name x =
    let nm : Module.named = M.with_namespace namespace name x in
    let t = Module.Dict.( union env.top (of_list [nm]) ) in
    debug "@[<hov 2>adding %s to@ @[[%a]@] yielding@ @[[%a]@]@]"
      name M.pp_mdict env.top M.pp_mdict t;
    top { env with top = t }

  let add_namespace env (nms:Namespaced.t) =
    let add x = top M.Dict.{ env with top = union env.top @@ of_list [x] } in
    debug "@[<v 2>Adding %a@; to %a@]@." Namespaced.pp nms pp_context
      env.current;
    if nms.namespace = [] then
      add (Modname.to_string nms.name, Link nms)
    else
      add (Module.namespace nms)

  let rec resolve_alias_md path def =
    match path with
    | [] -> None
    | a :: q ->
      match Name.Map.find a def with
      | M.Alias {path; _ } | M.Link path ->
        debug "resolved to %a" Namespaced.pp path;
        Some path
      | M.Sig m -> resolve_alias_sign q m.signature
      | M.Namespace n -> resolve_alias_md q n
      | M.Abstract _ | M.Fun _ -> None
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
    debug "resolving %a" Paths.S.pp path;
    match env.current with
    | In_namespace md -> resolve_alias_md path md
    | Signature sg -> resolve_alias_sign path sg

  let is_exterior path envt =
    match path with
    | [] -> false (* should not happen *)
    | a :: _ ->
      match find_name noloc Module a envt.current with
      | None -> true
      | Some m ->
        match m.main with
        | M.Namespace _
        | M.Sig { origin = Unit _; _ }
        | M.Link _  -> true
        | M.Alias _ -> false
        | M.Sig _ | M.Abstract _ | M.Fun _ -> false

  let expand_path path envt =
    match path with
    | [] -> []
    | a :: q ->
      match find_name noloc Module a envt.current with
      | None -> path
      | Some m ->
        match m.main with
        | M.Namespace _ -> path
        | M.Sig { origin = Unit {path=p; _ } ; _ } -> p.namespace @ (Modname.to_string p.name) :: q
        | M.Alias {path;_} -> Namespaced.flatten path @ q
        | _ -> path


  let pp ppf x = pp ppf x
end


let approx name =
  Module.mockup name ~path:{Pkg.source=Unknown; file=Namespaced.make name}

let open_world () =
  let mem = ref Name.Set.empty in
  let warn loc request =
    if Name.Set.mem request !mem then [] else begin
      mem := Name.Set.add request !mem;
      [unknown loc Module request]
    end in
  fun loc request ->
    debug "open world: requesting %s" request;
    return (M.md @@ approx request) <!> (warn loc request)

module Libraries = struct

  type source = {
    origin: Paths.Simple.t;
    mutable resolved: Core.t;
    cmis: Pkg.t Modname.Map.t
  }


  let read_dir dir =
    let files = Sys.readdir dir in
    let origin = Namespaced.filepath_of_filename dir in
    let cmis_map =
      Array.fold_left (fun m x ->
          if Filename.check_suffix x ".cmi" then
            let p =
              {Pkg.source = Pkg.Pkg origin; file = Namespaced.filepath_of_filename x} in
            Modname.Map.add (Pkg.module_name p) p m
          else m
        )
        Modname.Map.empty files in
    { origin=Namespaced.flatten origin; resolved= Core.start M.Def.empty; cmis= cmis_map }

  let create includes =  List.map read_dir includes

  module I = Dep_zipper.Outline(Core)
  let param = {
    Transforms.policy = Standard_policies.quiet;
    transparent_aliases = true;
    (* we are not recording anything *)
    transparent_extension_nodes = false;
    (* extension nodes should not appear in cmi *)
    epsilon_dependencies = false;
    (* do no try epsilon dependencies yet *)
  }

  let rec track source stack = match stack with
    | [] -> ()
    | (name, path, code) :: q ->
      let more = I.next ~pkg:path param source.resolved code in
      match more with
      | Error code ->
        begin match I.block code with
          | None -> assert false
          | Some { data = _y, bl_path ; _ } ->
            let name' = List.hd bl_path in
            let path' = Modname.Map.find (Modname.v name') source.cmis in
            let code' = I.initial (Cmi.m2l @@ Pkg.filename path') in
            let stack =
              (name', path', code') :: (name, path, code) :: q  in
            track source stack
        end
      | Ok (sg, _) ->
        let md = M.create
            ~origin:(M.Origin.Unit {source=path;path=Namespaced.make name}) sg in
        source.resolved <- Core.add_unit source.resolved name (M.Sig md);
        track source q

  let is_unknown = function
    | M.Sig { origin = Unit {source={ source = Unknown; _ };_};_} -> true
    | _ -> false

  let rec pkg_find name source =
    match Core.find_name noloc M.Module name source.resolved.current with
    | None ->
      let path = Modname.Map.find (Modname.v name) source.cmis in
      debug "pkg_find %S => %a." name Pkg.pp path;
      track source [name, path, I.initial (Cmi.m2l @@ Pkg.filename path) ];
      pkg_find name source
    | Some m -> let main = m.T.main in
      if is_unknown main then raise Not_found else main

  let rec pkgs_find name = function
    | [] -> raise Not_found
    | source :: q ->
      try pkg_find name source with Not_found -> pkgs_find name q

  let provider libs =
    let pkgs = create libs in
    fun _loc name ->
      debug "library layer: requesting %s" name;
      match pkgs_find name pkgs with
      | exception Not_found -> None
      | q -> return q
end
let libs = Libraries.provider

module Implicit_namespace = struct

  let provider (namespace,modules) =
    let open Query in
    let wrap x = match x.kind with
      | Mty M.Sig m -> M.Sig m
      | Namespace modules -> M.Namespace modules
      | Mty M.Abstract id -> M.Abstract id
      | Mty _ -> (* FIXME ?*) assert false
    in
    let env = Core.start (M.Def.modules modules) in
    let implicit loc path =
      Some(Core.find_implicit loc M.Module path env >>| wrap) in
    fun loc name ->
      try implicit loc [name] with Not_found ->
      try implicit loc (namespace @ [name]) with Not_found -> None

end
let implicit_namespace = Implicit_namespace.provider


let start ?(open_approximation=true) ~libs ~namespace ~implicits predefs =
  let empty = Core.start M.Def.empty in
  let files_in_namespace =
    List.fold_left Core.add_namespace empty namespace in
  let env =
    (* predefs should not override existing files *)
    Core.start @@ M.Def.modules
    @@ M.Dict.weak_union files_in_namespace.top predefs in
  let implicits = List.map implicit_namespace implicits in
  let libs = if not (libs = []) then [Libraries.provider libs] else [] in
  let open_approx = if open_approximation  then [open_world ()] else [] in
  { env with providers= libs @ implicits @ open_approx }
