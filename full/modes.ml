module Pkg = Paths.Pkg
open Params

type mode = Format.formatter -> Params.t -> Unit.r list Unit.pair -> unit

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
  | Export
  | Modules of variant * filter
  | Info
  | Signature
  | Sort


let info ppf _param {Unit.ml; mli} =
  let print =  Pp.(list ~sep:(s" @,") @@ Unit.pp ) ppf in
  print ml; print mli

let export ppf _param {Unit.mli; _} =
  let sign (u:Unit.r)= u.signature in
  let md (unit:Unit.r) =
    Module.M {Module.
      name = unit.name
    ; origin = Unit { source=Pkg.Special "exported"; file = [unit.name] }
    ; args = []
    ; signature = sign unit
    } in
  let s =
    let open Module.Sig in
    List.fold_left (fun sg u -> merge sg @@ create @@ md u) empty mli
  in
  Pp.fp ppf "@[<hov>let signature=@;\
             let open Module in @;\
             let open Sig in @;\
             %a\
             @]@." Module.reflect_signature s

let signature ppf _param {Unit.mli; _} =
  let md {Unit.signature; name; path; _  } =
    Module.M ( Module.create ~args:[]
      ~origin:(Unit path)
      name signature
             )
  in
  let mds = List.map md mli in
  let sexp = Sexp.( (list Module.sexp).embed ) mds in
  Pp.fp ppf "@[%a@]@." Sexp.pp sexp

let dependencies ?filter sort (u:Unit.r) =
  Pkg.Set.elements u.dependencies
  |> sort
  |> (match filter with
      | Some f -> List.filter f
      | None -> fun x -> x
    )
  |> List.map Pkg.module_name


let pp_module {Makefile.abs_path;slash; _ } proj ppf (u:Unit.r) =
  let pp_pkg = Pkg.pp_gen slash in
  let elts = proj u in
  Pp.fp ppf "%a: %a\n" pp_pkg (Common.make_abs abs_path u.path)
    Pp.( list ~sep:(s" ") Name.pp )
    elts

let aliases ppf param {Unit.mli; _ } =
  let mk_aliases (x:Unit.r) = Module.(aliases @@ M (create "" x.signature) ) in
  let param = param.makefile in
  let pp_pkg = Pkg.pp_gen param.slash in
  let pp_m (u:Unit.r) =
    let path = u.path in
    let path' = Pkg.update_extension
        (function "m2l" -> ".ml" | "m2li" -> ".mli" | s -> s ) path in
    let f = Common.make_abs param.abs_path path' in
    Pp.fp ppf "%a: %a\n" pp_pkg f
      Pp.( list ~sep:(s" ") Name.pp ) (mk_aliases u) in
  List.iter pp_m mli

let id x = x
let mname x = Pkg.module_name x
let upath x = mname @@ x.Unit.path

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

let modules ?filter =
  gen_modules (dependencies ?filter)


let pp_only_deps sort ?filter ppf u =
  let open Unit in
  let elts = Pkg.Set.elements u.dependencies in
  let elts = sort elts in
  let elts = match filter with
    | Some f -> List.filter f elts
    | None -> elts in
  Pp.fp ppf "%a"
    Pp.( list ~sep:(s"\n") Name.pp )
    ( List.map Pkg.module_name elts)

let line_modules ?filter ppf param {Unit.mli; ml } =
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
  @@ Pkg.Set.elements unit.Unit.dependencies


let dot ppf param {Unit.mli; _ } =
  let open Unit in
  let sort = sort mname param mli in
  Pp.fp ppf "digraph G {\n";
  List.iter (fun u ->
      List.iter (fun p ->
          Pp.fp ppf "%s -> %s \n" u.name @@ Pkg.module_name p)
        (local_dependencies sort u)
    ) mli;
  Pp.fp ppf "}\n"

let local_deps x =
  let filter = function { Pkg.source = Local; _ } -> true | _ -> false in
  x.Unit.dependencies |> Pkg.Set.filter filter
  |> Pkg.Set.elements
  |> Pkg.Set.of_list


let sort ppf _param (units: _ Unit.pair) =
  let gs = Unit.Groups.R.group units in
  let extract_path _ g l = match g with
    | { Unit.ml = Some x; mli = _ }
    | { ml = None; mli = Some x }  -> x.Unit.path :: l
    | { ml = None; mli = None } -> l in
  let paths =
    Paths.S.Map.fold extract_path gs []  in
  let deps path =
    let key = path.Pkg.file in
    match Unit.Groups.R.Map.find key gs with
    | { ml = Some x; mli = Some y } ->
      if path = x.path then
        let (+) = Pkg.Set.union in
        (local_deps x) + (local_deps y) + (Pkg.Set.singleton y.path)
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

  let dep = function
    | { Pkg.source = (Unknown|Local); _ } -> true
    |  _ -> false


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
  | Export -> export
  | Modules (Standard, filter) -> modules ~filter:(Filter.eval filter)
  | Modules (Nl, filter) -> line_modules ~filter:(Filter.eval filter)
  | Info -> info
  | Signature ->  signature
  | Sort -> sort
