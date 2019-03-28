module Pkg = Paths.Pkg
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
  | Aliases
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
let ufile (u:Unit.r) = str Paths.Pkg.pp u.src
let upath (u:Unit.r) = Namespaced.flatten u.path

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
    let path = upath u in
    LocalSet.add
      { path; ml = Option.fmap ufile x.ml ; mli = Option.fmap ufile x.mli }
      set
  | _ -> set

let build_atlas (lib,unknow) (u:Unit.r) =
  let build_atlas {Deps.path; pkg; _} (lib,unknw)  =
    let add_libs lib path = LibSet.add { Schema.lib; path } in
    match pkg.source with
    | Paths.P.(Local|Special _) -> lib, unknw
    | Paths.P.Pkg pkg' -> add_libs pkg' path lib, unknw
    | Paths.P.Unknown -> lib, Paths.S.Set.add path unknw in
  Deps.fold build_atlas (Unit.deps u) (lib,unknow)

let structured fmt _ _ ppf param units =
  let fmt = Option.default param.external_format fmt in
  let all = units.Unit.mli @ units.ml in
  let pp = let open Schematic in
    match fmt with Json -> Ext.json Schema.x | Sexp -> Ext.sexp Schema.x in
  let lib, unknown =
    List.fold_left build_atlas (LibSet.empty, Paths.S.Set.empty) all in
  let groups = Unit.Group.group units in
  let local =
    Paths.S.Map.fold (fun _ x set -> assoc x set) groups LocalSet.empty in
  let dep (u:Unit.r) =
    { Schema.file = ufile u; deps= Deps.paths (Unit.deps u) } in
  let dependencies = List.map dep all in
  let local = LocalSet.elements local in
  let library = LibSet.elements lib in
  let unknown = Paths.S.Set.elements unknown in
  Pp.fp ppf "%a@." pp {dependencies;local; library; unknown}


let export name _ _ ppf _param {Unit.mli; _} =
  (* TODO: prefixed unit *)
  let sign (u:Unit.r)= Unit.signature u in
  let md (unit:Unit.r) =
    let fp = Namespaced.flatten unit.path in
    Module.M {Module.
      name = unit.path.name
    ; origin =
        Unit { source = { source=Pkg.Special name; file = fp };
               path = fp
             }
    ; args = []
    ; signature = sign unit
    } in
  let s =
    List.fold_left (fun sg u -> Name.Map.union' sg @@ Module.Dict.of_list[md u])
      Module.Dict.empty mli
  in
  Pp.fp ppf "@[<hov>let modules=@;\
             let open Module in @;\
             let open Sig in @;\
             %a @]@." Module.reflect_modules s

let signature filename writer ppf param {Unit.mli; _} =
  (* TODO: prefixed unit *)
  let md (u:Unit.r) =
    let origin =
      Module.Origin.Unit {source=u.src;path=Namespaced.flatten u.path} in
    Module.M ( Module.create ~args:[] ~origin u.path.name (Unit.signature u ))
  in
  let mds = List.map md mli in
  writer.Io.sign param.internal_format filename ppf mds


let dependencies ?filter sort (u:Unit.r) =
  let filter = match filter with
    | Some f -> List.filter f
    | None -> fun x -> x in
  u
  |> Unit.deps
  |> Deps.pkgs
  |> sort
  |> filter
  |> List.map Pkg.module_name


let pp_module {Makefile.abs_path;slash; _ } proj ppf (u:Unit.r) =
  let pp_pkg = Pkg.pp_gen slash in
  let elts = proj u in
  Pp.fp ppf "%a: %a\n" pp_pkg (Common.make_abs abs_path u.src)
    Pp.( list ~sep:(s" ") Name.pp )
    elts

let aliases _ _ ppf param {Unit.mli; _ } =
  let mk_aliases (x:Unit.r) =
    Module.(aliases @@ M (create "" @@ Unit.signature x) ) in
  let param = param.makefile in
  let pp_pkg = Pkg.pp_gen param.slash in
  let pp_m (u:Unit.r) =
    let path = u.src in
    let path' = Pkg.update_extension
        (function "m2l" -> ".ml" | "m2li" -> ".mli" | s -> s ) path in
    let f = Common.make_abs param.abs_path path' in
    Pp.fp ppf "%a: %a\n" pp_pkg f
      Pp.( list ~sep:(s" ") Namespaced.pp ) (mk_aliases u) in
  List.iter pp_m mli

let mname x = Namespaced.make @@ Pkg.module_name x
let upath x = mname @@ x.Unit.src

module Hidden = struct
let sort proj _param mli =
  let order = Sorting.remember_order mli in
  Sorting.toposort order proj
end
open Hidden

let gen_modules proj ppf param {Unit.mli; ml } =
  let sort_p = sort mname param mli in
  let sort_u = sort upath param mli in
  let print units = Pp.fp ppf "%a"
      Pp.(list ~sep:(s"") @@ pp_module param.makefile @@ proj sort_p)
      (sort_u units) in
  print ml; print mli

let modules ?filter _ _ =
  gen_modules (dependencies ?filter)


let pp_only_deps sort ?filter ppf u =
  let open Unit in
  let elts = Deps.pkgs (Unit.deps u) in
  let elts = sort elts in
  let elts = match filter with
    | Some f -> List.filter f elts
    | None -> elts in
  Pp.fp ppf "%a"
    Pp.( list ~sep:(s"\n") Name.pp )
    ( List.map Pkg.module_name elts)

let line_modules ?filter _ _ ppf param {Unit.mli; ml } =
  let sort_p = sort mname param mli in
  let sort_u = sort upath param mli in
  let print units = Pp.fp ppf "%a"
      Pp.(list ~sep:(s"") @@ pp_only_deps sort_p ?filter)
      (sort_u units) in
  print ml; print mli

let local_dependencies sort unit =
  sort
  @@ List.filter
    (function {Pkg.source=Unknown; _ }
            | {Pkg.source=Special _ ; _ } -> false | _ -> true )
  @@ Deps.pkgs @@ Unit.deps unit


let dot _ _ ppf param {Unit.mli; _ } =
  let escaped = Name.Set.of_list
      [ "graph"; "digraph"; "subgraph"; "edge"; "node"; "strict" ] in
  let escape ppf s =
    if Name.Set.mem (String.lowercase_ascii s) escaped then
      Pp.fp ppf {|"%s"|} s
    else
      Pp.string ppf s in
  let open Unit in
  let sort = sort mname param mli in
  Pp.fp ppf "digraph G {\n";
  List.iter (fun u ->
      List.iter (fun p ->
          Pp.fp ppf "%a -> %a \n"
            escape (Namespaced.to_string u.path)
            escape (Pkg.module_name p)
        )
        (local_dependencies sort u)
    ) mli;
  Pp.fp ppf "}\n"


let local_deps x =
  let filter = function { Pkg.source = Local; _ } -> true | _ -> false in
  x |> Unit.deps |> Deps.pkg_set |> Pkg.Set.filter filter


let sort _ _ ppf _param (units: _ Unit.pair) =
  let module G = Unit.Group in
  let gs = G.group units in
  let flat g = fst @@ G.flatten g
  (* errors should have handled earlier *) in
  let extract_path _ g l = match flat g with
    | { Unit.ml = Some x; mli = _ }
    | { ml = None; mli = Some x }  -> x.Unit.src :: l
    | { ml = None; mli = None } -> l in
  let paths =
    Paths.S.Map.fold extract_path gs []  in
  let deps path =
    let key = path.Pkg.file in
    match flat @@ G.Map.find key gs with
    | { ml = Some x; mli = Some y } ->
      if path = x.src then
        let (+) = Pkg.Set.union in
        (local_deps x) + (local_deps y) + (Pkg.Set.singleton y.src)
      else
        local_deps y
    | { ml = Some x; mli = None } | { mli= Some x; ml =None } -> local_deps x
    | { ml = None; mli = None } -> Pkg.Set.empty in
  Option.iter (Pp.list ~sep:Pp.(s" ") ~post:Pp.(s"\n") Pkg.pp ppf)
    (Sorting.full_topological_sort deps paths)


module Filter = struct
  let inner = function
    | { Pkg.source = Local; _ } -> true
    |  _ -> false

  let dep = fun _ -> true

  let extern = function
    | { Pkg.source = Unknown; _ } -> true
    | _ -> false

  let lib = function
    | { Pkg.source = (Pkg _ | Special _ ) ; _ } -> true
    | _ -> false

  let eval = function
    | Inner -> inner
    | Dep -> dep
    | Extern -> extern
    | Lib -> lib
end



let eval = function
  | Aliases -> aliases
  | Dot -> dot
  | Export name -> export name
  | Modules (Standard, filter) -> modules ~filter:(Filter.eval filter)
  | Modules (Nl, filter) -> line_modules ~filter:(Filter.eval filter)
  | Info -> info
  | Deps f -> structured f
  | Signature ->  signature
  | Sort -> sort
