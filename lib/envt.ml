module M = Module
module Edge = Deps.Edge
module P = Paths.Pkg
module Out = Outliner
module Y = Summary

let debug fmt = Format.ifprintf Pp.err ("Debug:" ^^ fmt ^^"@.")

type answer = Out.answer =
  | M of Module.m
  | Namespace of Module.namespace_content

type context =
  | Signature of M.signature
  | In_namespace of M.dict

module Query = struct

  type 'a t = 'a Outliner.query_result
  let pure main = { Outliner.main; msgs = [] }
  let create main msgs : _ t = {main; msgs}

  let (>>|) x f = { x with Outliner.main = f x.Outliner.main }
  let (>>?) (x: _ t option) f =
    Option.( x >>= fun x ->
             f x.main >>| fun q ->
             { q with Out.msgs = q.Out.msgs @ x.msgs }
           )

  let add_msg msgs (q: _ t) = { q with msgs = msgs @ q.msgs }

end

type module_provider = Name.t -> Module.t Query.t option
let last l = List.hd @@ List.rev l

let to_context s = Signature (Exact s)

let ambiguity name breakpoint =
  let f = Standard_faults.ambiguous in
  { f  with
    Fault.log = (fun lvl l -> f.log lvl l name breakpoint)
  }

let nosubmodule current level name =
  let fault = Standard_faults.nonexisting_submodule in
  {  fault with
     Fault.log = (fun lvl l ->
         fault.log lvl l (List.rev current) level name
       )
  }

let unknown mlvl path =
  let fault = Standard_faults.unknown_approximated in
  {  fault with
     Fault.log = (fun lvl -> fault.log lvl mlvl path)
  }


module Core = struct

  type t = {
    top: M.Dict.t;
    current: context;
    deps: Deps.t ref;
    providers: module_provider list;
  }

  let empty = {
    top = Name.Map.empty;
    current = Signature(Exact(M.Def.empty));
    deps = ref P.Map.empty;
    providers = []
  }

  let eq x y= x.top = y.top
  let start s =
    { top = s.M.modules; current = to_context s;
      deps = ref P.Map.empty;
      providers = []
    }

  let pp_context ppf = function
    | In_namespace modules ->
      Pp.fp ppf "namespace [%a]@." Module.pp_mdict modules
    | Signature sg -> Pp.fp ppf "[%a]@." Module.pp_signature sg

  let pp ppf x = Pp.fp ppf "@[top=%a@ context=%a@]"
      M.pp_mdict x.top pp_context x.current

  module D = struct
    let path_record edge mp p env =
      env.deps := Deps.update p edge (Paths.S.Set.singleton mp) !(env.deps)

    let phantom_record name env =
      path_record Edge.Normal [name] { P.source = Unknown; file = [name] } env


    let record edge root env (m:Module.m) =
      match m.origin with
      | M.Origin.Unit p ->
        path_record edge p.path p.source env; []
      | Phantom (phantom_root, b) ->
        if root && not phantom_root then
          (phantom_record m.name env; [ambiguity m.name b] ) else []
      | _ -> []
  end open D

  let request lvl name env =
    let rec request name  = function
      | [] -> None
      | f :: q ->
        match f name with
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
    | x -> Some x

  let rec find_name level name current =
    match current with
    | Signature Module.Blank ->
      (* we are already in error mode here, no need to emit yet another warning *)
      Some (Query.pure @@ M.md @@ M.mockup name)
    | In_namespace modules ->
      if level = M.Module_type then None
      else Option.fmap Query.pure @@ find_opt name modules
    | Signature Exact def ->
      Option.fmap Query.pure @@ find_opt name @@ proj level def
    | Signature Divergence d ->
      (* If we have a divergent signature, we first look
         at the signature after the divergence: *)
      match find_opt name @@ proj level d.after with
      | Some x -> Some(Query.pure x)
      | None ->
        let open Query in
        let (|||) = Option.(|||) in

        (* We then try to find the searched name in the signature
           before the divergence *)
        begin find_name level name (Signature d.before) >>? fun q ->
          Some (Query.create
                  (Module.spirit_away d.point q)
                  [ambiguity name d.point])
          (* If we found the expected name before the divergence,
              we add a new message to the message stack, and return
              the found module, after marking it as a phantom module. *)

        end
        (* If we did not find anything and were looking for a module type,
           we return a mockup module type *)
        ||| lazy (if level = Module_type then
                    Some(Query.create (M.md @@ M.mockup name)
                           [unknown Module_type name] )
                  else None)


  let rec find
      ~approx_submodule
      current
      ~absolute_path ?(edge=Edge.Normal)
      ~root level
      path
      env =
    debug "looking for %a" Paths.S.pp path;
    debug "in %a, sub-approx: %B" pp_context env.current approx_submodule;
    match path with
    | [] ->
      raise (Invalid_argument "Envt.find cannot find empty path")
    | a :: q ->
      let lvl = adjust_level level q in
      let open Query in
      let r =
        match find_name lvl a env.current with
        | None when (not root || lvl = Module_type) && approx_submodule ->
          begin
            debug "submodule approximate %s" (last path);
            Some (create (M.md @@ M.mockup @@ last path)
                    [nosubmodule current lvl a])
          end
        | None -> request lvl a env
        | Some _ as x -> x in
      r >>?
      begin debug "found %s" a;
      function
      | Alias {path; phantom; name; weak = false } ->
            debug "alias to %a" Namespaced.pp path;
            let msgs =
              match phantom with
              | None -> []
              | Some b ->
                if root then
                  (phantom_record name env; [ambiguity name b])
                else [] in
            (* aliases link only to compilation units *)
            Option.(
              find (a::current)
                ~approx_submodule
                ~absolute_path:false ~root:true ~edge
                level (Namespaced.flatten path @ q) (top env)
              >>| Query.add_msg msgs
            )
      | Alias { weak = true; _ } when absolute_path -> None
      | Alias {path; weak = true; _ } ->
        find [] ~absolute_path:true ~approx_submodule ~root:true ~edge level
          (Namespaced.flatten path @ q) (top env)
      | M.M m ->
        debug "found module %s" m.name;
        begin
          let faults = record edge root env m in
          if q = [] then
            Some ((create (M m) faults))
          else
            find (a::current) ~approx_submodule
              ~absolute_path:false ~root:false level q
            @@ restrict env @@ Signature m.signature
        end
      | Namespace {name;modules} ->
        begin
          (*          let faults = record edge root env name in*)
          if q = [] then
            Some (Query.pure (Namespace {name;modules}))
          else
            find (a::current) ~approx_submodule ~absolute_path
              ~root:true level q
            @@ restrict env @@ In_namespace modules

        end
      end

  let find sub ?edge level path envt =
    match find [] ~approx_submodule:sub ~absolute_path:false
            ?edge ~root:true level path envt with
    | None -> raise Not_found
    | Some x -> x

  let find_implicit = find false
  let find = find true

  let deps env = !(env.deps)
  let reset_deps env = env.deps := P.Map.empty

  let to_sign = function
    | Signature s -> s
    | In_namespace modules ->
      M.Exact { M.Def.empty with modules }

  let (>>) env def =
    restrict env @@
    Signature (Y.extend (to_sign env.current) def)

  let add_unit env ?(namespace=[]) x =
    let m: Module.t = M.with_namespace namespace x in
    let t = Module.Dict.( union env.top (of_list [m]) ) in
    debug "@[<hov 2>adding %s to@ @[[%a]@] yielding@ @[[%a]@]@]"
      (M.name m) M.pp_mdict env.top M.pp_mdict t;
    top { env with top = t }

  let add_namespace env (nms:Namespaced.t) =
    let add x = top M.Dict.{ env with top = union env.top @@ of_list [x] } in
    debug "@[<v 2>Adding %a@; to %a@]@." Namespaced.pp nms pp_context
      env.current;
    if nms.namespace = [] then
      add (M.Alias { name= nms.name; path=nms; phantom = None; weak = true })
    else
      add (Module.namespace nms)

  let rec resolve_alias_md path def =
    match path with
    | [] -> None
    | a :: q ->
      match Name.Map.find a def with
      | M.Alias {path; _ } ->
        debug "resolved to %a" Namespaced.pp path;
        Some path
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
    debug "resolving %a" Paths.S.pp path;
    match env.current with
    | In_namespace md -> resolve_alias_md path md
    | Signature sg -> resolve_alias_sign path sg

  let is_exterior path envt =
    match path with
    | [] -> false (* should not happen *)
    | a :: _ ->
      match find_name Module a envt.current with
      | None -> true
      | Some m ->
        match m.main with
        | Namespace _ -> true
        | M { origin = Unit _; _ } -> true
        | M.Alias a -> a.weak
        | _ -> false

  let expand_path path envt =
    match path with
    | [] -> []
    | a :: q ->
      match find_name Module a envt.current with
      | None -> path
      | Some m ->
        match m.main with
        | Namespace _ -> path
        | M { origin = Unit {path=p; _ } ; _ } -> p @ q
        | M.Alias {path;_} -> Namespaced.flatten path @ q
        | _ -> path


  let pp ppf x = pp ppf x
end

let approx name =
  Module.mockup name ~path:{Paths.P.source=Unknown; file=[name]}

let open_world () =
  let mem = ref Name.Set.empty in
  let warn request =
    if Name.Set.mem request !mem then [] else
      (mem := Name.Set.add request !mem; [unknown Module request] ) in
  fun request ->
    debug "open world: requesting %s" request;
    Some (Query.create(M.md @@ approx request) (warn request)  )

module Libraries = struct

  type source = {
    origin: Paths.Simple.t;
    mutable resolved: Core.t;
    cmis: P.t Name.map
  }


  let read_dir dir =
    let files = Sys.readdir dir in
    let origin = Paths.S.parse_filename dir in
    let cmis_map =
      Array.fold_left (fun m x ->
          if Filename.check_suffix x ".cmi" then
            let p =
              {P.source = P.Pkg origin; file = Paths.S.parse_filename x} in
            Name.Map.add (P.module_name p) p m
          else m
        )
        Name.Map.empty files in
    { origin; resolved= Core.start M.Def.empty; cmis= cmis_map }

  let create includes =  List.map read_dir includes

  module I = Outliner.Make(Core)(struct
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
            ~origin:(M.Origin.Unit {source=path;path=[name]}) name sg in
        source.resolved <- Core.add_unit source.resolved (M.M md);
        track source q

  let rec pkg_find name source =
    match Core.find_name M.Module name source.resolved.current with
    | Some {main =
              M.M { origin = Unit {source={ source = Unknown; _ }; _ }; _ }; _} ->
      raise Not_found
    | None ->
      let path = Name.Map.find name source.cmis in
      track source
        [name, path, Cmi.m2l @@ P.filename path ];
      pkg_find name source
    | Some m -> m.Out.main

  let rec pkgs_find name = function
    | [] -> raise Not_found
    | source :: q ->
      try
        let m = pkg_find name source in
        m
      with Not_found ->
        pkgs_find name q

  let provider libs =
    let pkgs = create libs in
    fun name ->
      debug "library layer: requesting %s" name;
      match pkgs_find name pkgs with
      | exception Not_found -> None
      | q -> Some (Query.pure q)
end
let libs = Libraries.provider

module Implicit_namespace = struct

  let provider (namespace,modules) =
    let open Query in
    let wrap = function
        | M m -> M.M m
        | Namespace {name;modules} ->
          M.Namespace {name; modules} in
    let env = Core.start (M.Def.modules modules) in
    fun name ->
      try
          Some(Core.find_implicit M.Module [name] env >>| wrap)
      with Not_found ->
      try
        Some(Core.find_implicit M.Module (namespace @ [name]) env >>| wrap)
      with Not_found -> None

end
let implicit_namespace = Implicit_namespace.provider


let start ?(open_approximation=true)
    ~libs ~namespace ~implicits predefs  =
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
