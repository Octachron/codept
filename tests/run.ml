module Pth = Paths.Pkg

let local file = Pth.local @@ Paths.S.parse_filename file

let organize files =
  let add_name m n  =  Name.Map.add (Unit.extract_name n) (local n) m in
  let m = List.fold_left add_name
      Name.Map.empty (files.Unit.ml @ files.mli) in
  let units = Unit.( split @@ group files ) in
  units, m


let start_env filemap =
  let layered = Envts.Layered.create [] @@ Stdlib.signature in
  let traced = Envts.Trl.extend layered in
  Envts.Tr.start traced filemap

module Param = struct
  let all = false
  let native = false
  let bytecode = false
  let abs_path = false
  let sort = false
  let slash = Filename.dir_sep
  let transparent_aliases = true
  let transparent_extension_nodes = true
  let includes = Name.Map.empty
  let implicits = true
  let no_stdlib = false
end

let analyze files =
  let units, filemap = organize files in
  let module Envt = Envts.Tr in
  let core = start_env filemap in
  let module S = Solver.Make(Param) in
  let {Unit.ml; mli} =
    try S.resolve_split_dependencies core units with
      S.Cycle (_env,units) ->
      Error.log "%a" Solver.Failure.pp_cycle units
  in
  { Unit.ml; mli }

let normalize set =
  set
  |> Pth.Set.elements
  |> List.map Pth.module_name
  |> List.sort compare


let (%=%) list set =
  normalize set = List.sort compare list

let deps_test {Unit.ml; mli} =
  let module M = Paths.S.Map in
  let build exp = List.fold_left (fun m (x,l) ->
      M.add (Paths.S.parse_filename x) l m)
      M.empty exp in
  let exp = M.union' (build ml) (build mli) in
  let files = { Unit.ml = List.map fst ml; mli =  List.map fst mli} in
  let {Unit.ml; mli} = analyze files in
  let (=?) expect files = List.for_all (fun u ->
      let path = u.Unit.path.Pth.file in
      let expected =
          Paths.S.Map.find path expect
      in
      let r = expected %=% u.Unit.dependencies in
      if not r then
        Pp.p "Failure %a: expected:[%a], got:@[[%a]@]\n"
          Pth.pp u.Unit.path
          Pp.(list estring) (List.sort compare expected)
          Pp.(list estring) (normalize u.Unit.dependencies);
      r
    ) files in
  exp =? ml && exp =? mli

let ml_only ml = { Unit.mli = []; ml }

let result =
  Sys.chdir "tests";
  List.for_all deps_test [
    ml_only ["abstract_module_type.ml", []];
    ml_only ["alias_map.ml", ["Aliased__B"; "Aliased__C"] ];
    ml_only ["apply.ml", ["F"; "X"]];
    ml_only ["basic.ml", ["Ext"; "Ext2"]];
    ml_only ["bindings.ml", []];
    ml_only ["bug.ml", ["Sys"] ];
    ml_only ["case.ml", ["A"; "B";"C";"D";"F"]];
    ml_only ["even_more_functor.ml", ["E"; "A"]];
    ml_only ["first-class-modules.ml", ["Mark";"B"] ];
    ml_only ["first_class_more.ml", [] ];
    ml_only ["functor.ml", [] ];
    ml_only ["functor_with_include.ml", [] ];
    ml_only ["include.ml", ["List"] ];
    ml_only ["include_functor.ml", ["A"] ];
    ml_only ["letin.ml", ["List"] ];
    ml_only ["module_rec.ml", ["Set"] ];
    ml_only ["more_functor.ml", ["Ext";"Ext2"] ];
    ml_only ["nested_modules.ml", [] ];
    ml_only ["no_deps.ml", [] ];
    ml_only ["opens.ml", ["A";"B"] ];
    ml_only ["pattern_open.ml", ["E1"; "E2"; "E3";"E4"] ];
    ml_only ["recmods.ml", ["Ext"]];
    ml_only ["record.ml", ["Ext";"E2";"E3"]];
    ml_only ["simple.ml", ["G";"E"; "I"; "A"; "W"; "B"; "C"; "Y"; "Ext"]];


  ]

let () =
  if result then
    Format.printf "Success.\n"
  else
    Format.printf "Failure.\n"
