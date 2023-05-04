open Params

type mode =
  string -> Io.writer ->
  Format.formatter -> Params.t -> Unit.r list Unit.pair -> unit

type filter =
  | Inner
  | Extern
  | Lib
  | Dep

type variant =
  | Standard
  | Nl

type t =
  | Dot
  | Export of Name.t
  | Modules of variant * filter
  | Info
  | Deps of Schematic.format option
  | Signature
  | Sort


let info _ _ ppf _param {Unit.ml; mli} =
  let print =  Pp.(list ~sep:(s" @,") @@ Unit.pp ) ppf in
  print ml; print mli

let str x = Format.asprintf "%a" x
let ufile (u:Unit.r) = str Pkg.pp u.src

module LocalSet =
  Set.Make(struct type t = Schema.local_association let compare=compare end)
module LibSet =
  Set.Make(struct type t = Schema.library_module let compare=compare end)

let assoc x set =
  let x, _  = Unit.Group.flatten x in
  let x = match x.mli, x.ml with
    | Some ({ kind = M2l.Structure; _ } as u) , None ->
      { Unit.ml = Some u; mli = None }
    | _ -> x in
  match x.mli, x.ml with
  | Some u, _ | None, Some u ->
    let path = u.path in
    LocalSet.add
      { path; ml = Option.fmap ufile x.ml ; mli = Option.fmap ufile x.mli }
      set
  | _ -> set

let build_atlas (lib,unknow) (u:Unit.r) =
  let build_atlas {Deps.path; pkg; _} (lib,unknw)  =
    let add_libs lib path = LibSet.add { Schema.lib; path } in
    match pkg.source with
    | Pkg.Local | Pkg.Special _ -> lib, unknw
    | Pkg.Pkg pkg' -> add_libs pkg' path lib, unknw
    | Pkg.Unknown -> lib, Namespaced.Set.add path unknw in
  Deps.fold build_atlas (Unit.deps u) (lib,unknow)

let structured fmt _ _ ppf param units =
  let fmt = Option.default param.external_format fmt in
  let all = units.Unit.mli @ units.ml in
  let pp = let open Schematic in
    match fmt with Json -> Ext.json Schema.x | Sexp -> Ext.sexp Schema.x in
  let lib, unknown =
    List.fold_left build_atlas (LibSet.empty, Namespaced.Set.empty) all in
  let groups = Unit.Group.group units in
  let local = Unit.Group.Map.fold assoc groups LocalSet.empty in
  let dep (u:Unit.r) =
    { Schema.file = ufile u; deps= Deps.paths (Unit.deps u) } in
  let dependencies = List.map dep all in
  let local = LocalSet.elements local in
  let library = LibSet.elements lib in
  let unknown = Namespaced.Set.elements unknown in
  Pp.fp ppf "%a@." pp {dependencies;local; library; unknown}


let export name _ _ ppf _param {Unit.mli; _} =
  (* TODO: prefixed unit *)
  let sign (u:Unit.r)= Unit.signature u in
  let md (unit:Unit.r) =
    let uname = unit.path.file in
    let m = {
      Module.origin =
        Unit { source = { source=Pkg.Special name; file = unit.path };
               path = unit.path
             }
    ; signature = sign unit
    }
    in
    uname, Module.Sig m in
  let s =
    List.fold_left (fun sg u -> Name.Map.union' sg @@ Module.Dict.of_list[md u])
      Module.Dict.empty mli
  in
  Pp.fp ppf "@[<hov>let modules=@;\
             let open Module in @;\
             let open Sig in @;\
             %a @]@." Module.reflect_modules s

let signature _ writer ppf param {Unit.mli; _} =
  let md (u:Unit.r) =
    let origin =
      Module.Origin.Unit {source=u.src;path=u.path}
    in
    Module.Namespace.from_module u.path origin (Unit.signature u)
  in
  let mds = List.fold_left (fun s mli -> Module.Namespace.merge s (md mli))
      Module.Dict.empty mli in
  writer.Io.sign param.internal_format ppf mds


let dependencies ?filter (u:Unit.r) =
  let filter = match filter with
    | Some f -> List.filter f
    | None -> fun x -> x in
  u
  |> Unit.deps |> Deps.all
  |> filter

let pp_module {Makefile.abs_path;slash; _ } ?filter sort ppf (u:Unit.r) =
  let pp_pkg = Pkg.pp_gen slash in
  let pp_dep ppf d = Namespaced.pp ppf d.Deps.path in
  let elts = sort @@ dependencies ?filter u in
  Pp.fp ppf "%a: %a\n" pp_pkg (Common.make_abs abs_path u.src)
    Pp.( list ~sep:(s" ") pp_dep )
    elts

let mpath x = x.Deps.path
let upath x = Namespaced.make @@ Modname.to_string @@ Pkg.module_name x.Unit.src

module Hidden = struct
let sort mli naming =
  let order = Sorting.remember_order mli in
  Sorting.toposort order naming
end
open Hidden

let gen_modules ?filter ppf param {Unit.mli; ml } =
  let sort_p = sort mli mpath in
  let sort_u = sort mli upath in
  let print units =
    Pp.(list ~sep:(s"") @@ pp_module param.makefile ?filter sort_p)
      ppf
      (sort_u units)
  in
  print ml; print mli

let modules ?filter _ _ =
  gen_modules ?filter

let pp_only_deps sort ?filter ppf u =
  let elts = Deps.all @@ Unit.deps u in
  let elts = sort elts in
  let elts = match filter with
    | Some f -> List.filter f elts
    | None -> elts in
  Pp.fp ppf "%a"
    Pp.( list ~sep:(s"\n") Namespaced.pp )
    (List.map (fun d -> d.Deps.path) elts)

let line_modules ?filter _ _ ppf _param {Unit.mli; ml } =
  let sort_p = sort mli mpath in
  let sort_u = sort mli upath in
  let print units = Pp.fp ppf "%a"
      Pp.(list ~sep:(s"") @@ pp_only_deps sort_p ?filter)
      (sort_u units) in
  print ml; print mli

let dot _ _ ppf _param {Unit.mli; _ } =
  let escaped = Name.Set.of_list
      [ "graph"; "digraph"; "subgraph"; "edge"; "node"; "strict" ] in
  let escape ppf s =
    if Name.Set.mem (String.lowercase_ascii s) escaped then
      Pp.fp ppf {|"%s"|} s
    else
      Pp.string ppf s in
  let open Unit in
  let sort = sort mli mpath in
  Pp.fp ppf "digraph G {\n";
  List.iter (fun u ->
      List.iter (fun p ->
          Pp.fp ppf "%a -> %a \n"
            escape (Namespaced.to_string u.path)
            escape (Modname.to_string (Namespaced.module_name p.Deps.path))
        )
        (sort @@ Unit.local_dependencies u)
    ) mli;
  Pp.fp ppf "}\n"


let local_deps x =
  let filter x = match x.Deps.pkg.Pkg.source with Local -> Some (x.pkg, x.path) | _ -> None in
  x |> Unit.deps |> Deps.all |> Support.filter_map filter

let pp_cycle ppf path =
  Pp.fp ppf "Cycle detected: %a@."
    (Pp.list ~sep:Pp.(s" ") ~post:Pp.(s"\n") Pkg.pp)
    (List.map fst path)


let cycle_in_sort =
  Fault.info ["codept"; "sort"] "Cycle detected when sorting modules" pp_cycle

let mode_policy p  =
  Fault.register ~lvl:Fault.Level.critical cycle_in_sort p


let sort _ _ ppf param (units: _ Unit.pair) =
  let policy = mode_policy param.analyzer.policy in
  let module G = Unit.Group in
  let gs = G.group units in
  let flat g = fst @@ G.flatten g
  (* errors should have handled earlier *) in
  let extract_path g l = match flat g with
    | { Unit.ml = Some x; mli = _ }
    | { ml = None; mli = Some x }  -> (x.Unit.src, x.Unit.path) :: l
    | { ml = None; mli = None } -> l in
  let paths = G.Map.fold extract_path gs []  in
  let deps (src, path) =
    let key = path in
    match flat @@ G.Map.find key gs with
    | { ml = Some x; mli = Some y } ->
      if src = x.src then
        (local_deps x) @ (local_deps y)
        @ [y.src, y.path]
      else
        local_deps y
    | { ml = Some x; mli = None } | { mli= Some x; ml =None } -> local_deps x
    | { ml = None; mli = None } -> [] in
  let sorted = Sorting.full_topological_sort ~key:fst deps paths in
  match sorted with
  | Error path ->
    Fault.raise policy cycle_in_sort path
  | Ok sorted ->
    Pp.fp ppf "%a"
     (Pp.list ~sep:Pp.(s" ") ~post:Pp.(s"\n") Pkg.pp)
    (List.map fst sorted)


module Filter = struct
  let inner x = match x.Deps.pkg with
    | { Pkg.source = Local; _ } -> true
    |  _ -> false

  let dep = fun _ -> true

  let extern  x = match x.Deps.pkg with
    | { Pkg.source = Unknown; _ } -> true
    | _ -> false

  let lib  x = match x.Deps.pkg with
    | { Pkg.source = (Pkg _ | Special _ ) ; _ } -> true
    | _ -> false

  let eval = function
    | Inner -> inner
    | Dep -> dep
    | Extern -> extern
    | Lib -> lib
end



let eval = function
  | Dot -> dot
  | Export name -> export name
  | Modules (Standard, filter) -> modules ~filter:(Filter.eval filter)
  | Modules (Nl, filter) -> line_modules ~filter:(Filter.eval filter)
  | Info -> info
  | Deps f -> structured f
  | Signature ->  signature
  | Sort -> sort
