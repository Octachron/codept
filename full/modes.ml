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
  | Json
  | Sexp
  | Signature
  | Sort


let info _ _ ppf _param {Unit.ml; mli} =
  let print =  Pp.(list ~sep:(s" @,") @@ Unit.pp ) ppf in
  print ml; print mli

let str x = Format.asprintf "%a" x
let ufile (u:Unit.r) = str Paths.Pkg.pp u.src
let upath (u:Unit.r) = Namespaced.flatten u.path

let structured pp _ _ ppf _ units =
  let udeps (u:Unit.r) =
      let add_dep (loc,lib,unknw) (p:Paths.P.t) =
        match p.source with
        | Paths.P.Local -> p.file :: loc, lib, unknw
        | Paths.P.Pkg pkg ->
          let p: _ Scheme.tuple = [pkg; p.file] in
          loc, p :: lib, unknw
        | Paths.P.Unknown -> loc, lib, p.file :: unknw
        | Paths.P.Special _ -> loc, lib, unknw in
      List.fold_left add_dep ([],[],[]) (Deps.Forget.to_list u.dependencies) in

  let open Scheme in
  let open Schema in
  let groups = Unit.Groups.R.group units in
  let assoc (_, x) =
    let x, _  = Unit.Groups.R.flatten x in
    let m, ml, mli = Schema.(m,ml,mli) in
    let x = match x.mli, x.ml with
      | Some ({ kind = M2l.Structure; _ } as u) , None ->
        { Unit.ml = Some u; mli = None }
      | _ -> x in
    match x.mli, x.ml with
    | Some u, _ | None, Some u ->
      L.[
        let p = upath u in
        obj [ m $= p;
              ml $=? Option.fmap ufile x.ml;
              mli $=? Option.fmap ufile x.mli
            ]
      ]
    | _ -> [] in
  let atl =
    List.fold_left(fun l x -> assoc x @ l)[](Paths.S.Map.bindings groups) in
  let dep (u:Unit.r) =
    let loc, libs, unkns =  udeps u in
    let wrap l = if l = L.[] then None else Some l in
    let all_deps = obj [ local $= loc; lib $=? wrap libs; unknown $=? wrap unkns ] in
    obj [ file $= ufile u; dependencies $= all_deps ] in
  let ud = List.map dep units.ml @ List.map dep units.mli in
  let data = let open Scheme in
    obj [ atlas $= atl; dependencies $= ud ] in
  Pp.fp ppf "%a@." (pp schema) data


let export name _ _ ppf _param {Unit.mli; _} =
  (* TODO: prefixed unit *)
  let sign (u:Unit.r)= u.signature in
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

let signature filename writer ppf _param {Unit.mli; _} =
  (* TODO: prefixed unit *)
  let md {Unit.signature; src; path; _  } =
    Module.M ( Module.create ~args:[]
      ~origin:(Unit {source=src;path=Namespaced.flatten path})
      path.name signature
             )
  in
  let mds = List.map md mli in
  writer.Io.sign filename ppf mds


let dependencies ?filter sort (u:Unit.r) =
  Pkg.Map.bindings u.dependencies
  |> List.map fst
  |> sort
  |> (match filter with
      | Some f -> List.filter f
      | None -> fun x -> x
    )
  |> List.map Pkg.module_name


let pp_module {Makefile.abs_path;slash; _ } proj ppf (u:Unit.r) =
  let pp_pkg = Pkg.pp_gen slash in
  let elts = proj u in
  Pp.fp ppf "%a: %a\n" pp_pkg (Common.make_abs abs_path u.src)
    Pp.( list ~sep:(s" ") Name.pp )
    elts

let aliases _ _ ppf param {Unit.mli; _ } =
  let mk_aliases (x:Unit.r) = Module.(aliases @@ M (create "" x.signature) ) in
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
  let elts = Deps.Forget.to_list u.dependencies in
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
  @@ Deps.Forget.to_list unit.Unit.dependencies


let dot _ _ ppf param {Unit.mli; _ } =
  let open Unit in
  let sort = sort mname param mli in
  Pp.fp ppf "digraph G {\n";
  List.iter (fun u ->
      List.iter (fun p ->
          Pp.fp ppf "%a -> %s \n"
            Namespaced.pp u.path @@ Pkg.module_name p)
        (local_dependencies sort u)
    ) mli;
  Pp.fp ppf "}\n"


let local_deps x =
  let filter = function { Pkg.source = Local; _ } -> true | _ -> false in
  x.Unit.dependencies |> Deps.Forget.to_set |> Pkg.Set.filter filter


let sort _ _ ppf _param (units: _ Unit.pair) =
  let module G = Unit.Groups.R in
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
  | Json -> structured Scheme.json
  | Sexp ->  structured Scheme.sexp
  | Signature ->  signature
  | Sort -> sort
