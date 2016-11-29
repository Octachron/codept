module Pth = Paths.Pkg

let local = Pth.local

let organize files =
  let add_name m n  =  Name.Map.add (Read.name n) (local n) m in
  let m = List.fold_left add_name
      Name.Map.empty (files.Unit.ml @ files.mli) in
  let units = Unit.( split @@ group files ) in
  units, m

module Envt = Envts.Tr

let start_env includes fileset filemap =
  let layered = Envts.Layered.create includes fileset @@ Stdlib.signature in
  let traced = Envts.Trl.extend layered in
  Envt.start traced filemap

module Param = struct
  let transparent_aliases = true
  let transparent_extension_nodes = true
end

module S = Solver.Make(Envt)(Param)

let analyze pkgs files =
  let units, filemap = organize files in
  let fileset = units.Unit.mli
                |> List.map (fun u -> u.Unit.name)
                |> Name.Set.of_list in
  let module Envt = Envts.Tr in
  let core = start_env pkgs fileset filemap in
    S.resolve_split_dependencies core units

let normalize set =
  set
  |> Pth.Set.elements
  |> List.map Pth.module_name
  |> List.sort compare


let simple_dep_test name list set =
  let r = normalize set = List.sort compare list in
  if not r then
    Pp.p "Failure %a: expected:[%a], got:@[[%a]@]\n"
      Pth.pp name
      Pp.(list estring) (List.sort compare list)
      Pp.(list estring) (normalize set);
  r


let (%) f g x = f (g x)

let normalize_2 set =
  let l = Pth.Set.elements set in
  let is_inner =
    function { Pth.source = Local; _ } -> true | _ -> false in
  let is_lib =
    function { Pth.source = (Pkg _ | Special _ ) ; _ } -> true
           | _ -> false in
  let inner, rest = List.partition is_inner l in
  let lib, unkn = List.partition is_lib rest in
  let norm = List.sort compare % List.map Pth.module_name in
  norm inner, norm lib, norm unkn


let precise_deps_test name (inner,lib,unkw) set =
  let sort = List.sort compare in
  let inner, lib, unkw = sort inner, sort lib, sort unkw in
  let inner', lib', unkw' = normalize_2 set in
  let test subtitle x y =
    let r = x = y  in
    if not r then
      Pp.p "Failure %a %s: expected:[%a], got:@[[%a]@]\n"
      Pth.pp name subtitle
      Pp.(list estring) x
      Pp.(list estring) y;
    r in
  test "local" inner inner'
  && test "lib" lib lib'
  && test "unknwon" unkw unkw'

let add_info {Unit.ml; mli} info = match Filename.extension @@ fst info with
  | ".ml" -> { Unit.ml = info :: ml; mli }
  | ".mli" -> { Unit.mli = info :: mli; ml }
  | _ -> raise (Invalid_argument "unknown extension")

let add_file {Unit.ml; mli} info = match Filename.extension @@ info with
  | ".ml" -> { Unit.ml = info :: ml; mli }
  | ".mli" -> { Unit.mli = info :: mli; ml }
  | _ -> raise (Invalid_argument "unknown extension")


let gen_deps_test libs inner_test l =
  let {Unit.ml;mli} = List.fold_left add_info {Unit.ml=[]; mli=[]} l in
  let module M = Paths.S.Map in
  let build exp = List.fold_left (fun m (x,l) ->
      M.add (Paths.S.parse_filename x) l m)
      M.empty exp in
  let exp = M.union' (build ml) (build mli) in
  let files = { Unit.ml = List.map fst ml; mli =  List.map fst mli} in
  let {Unit.ml; mli} = analyze libs files in
  let (=?) expect files = List.for_all (fun u ->
      let path = u.Unit.path.Pth.file in
      let expected =
          Paths.S.Map.find path expect
      in
      inner_test u.path expected u.dependencies
    ) files in
  exp =? ml && exp =? mli

let deps_test l =
  try gen_deps_test [] simple_dep_test l with
  | S.Cycle (_,units) ->
    Error.log "%a" Solver.Failure.pp_cycle units

let ocamlfind name =
  let cmd = "ocamlfind query " ^ name in
  let cin = Unix.open_process_in cmd in
  try
    [input_line cin]
  with
    End_of_file -> []

let cycle_test expected l =
    let files = List.fold_left add_file {Unit.ml=[]; mli=[]} l in
    try ignore @@ analyze [] files; false with
      S.Cycle (_,units) ->
      let open Solver.Failure in
      let map = analysis units in
      let cmap = categorize map in
      let cmap = normalize map cmap in
      let errs = Map.bindings cmap in
      let name unit = unit.Unit.name in
      let cycles = errs
                  |> List.filter (function (Cycle _, _) -> true | _ -> false)
                  |> List.map snd
                  |> List.map (List.map name % Unit.Set.elements) in
      let expected = List.sort compare expected in
      let cycles = List.sort compare cycles in
      let r = cycles = expected in
      if not r then
        ( Pp.fp Pp.std "Failure: expected %a, got %a\n"
            Pp.(list @@ list string) expected
            Pp.(list @@ list string) cycles;
          r )
      else
        r



let result =
  Sys.chdir "tests";
  List.for_all deps_test [
    ["abstract_module_type.ml", []];
    ["alias_map.ml", ["Aliased__B"; "Aliased__C"] ];
    ["apply.ml", ["F"; "X"]];
    ["basic.ml", ["Ext"; "Ext2"]];
    ["bindings.ml", []];
    ["bug.ml", ["Sys"] ];
    ["case.ml", ["A"; "B";"C";"D";"F"]];
    ["even_more_functor.ml", ["E"; "A"]];
    ["first-class-modules.ml", ["Mark";"B"] ];
    ["first_class_more.ml", [] ];
    ["foreign_arg_sig.ml", ["Ext";"Ext2"] ];
    ["functor.ml", [] ];
    ["functor_with_include.ml", [] ];
    [ "hidden.ml", ["Ext"] ];
    ["include.ml", ["List"] ];
    ["include_functor.ml", ["A"] ];
    ["letin.ml", ["List"] ];
    ["module_rec.ml", ["Set"] ];
    ["more_functor.ml", ["Ext";"Ext2"] ];
    ["nested_modules.ml", [] ];
    ["no_deps.ml", [] ];
    ["not_self_cycle.ml", ["E"] ];
    ["opens.ml", ["A";"B"] ];
    ["pattern_open.ml", ["E1"; "E2"; "E3";"E4"] ];
    ["recmods.ml", ["Ext"]];
    ["record.ml", ["Ext";"E2";"E3"]];
    ["simple.ml", ["G";"E"; "I"; "A"; "W"; "B"; "C"; "Y"; "Ext"]];
    ["solvable.ml", ["Extern"]];
    ["tuple.ml", ["A"; "B"; "C"]];
    ["unknown_arg.ml", ["Ext"] ];
    ["with.ml", ["Ext"] ]


  ]
  &&( Sys.chdir "mixed";
      deps_test ["a.ml", ["D"];
                 "a.mli", ["D";"B"];
                 "b.ml", ["C"];
                 "c.mli", ["D"];
                 "d.mli", []
                ]
    )
  && ( Sys.chdir "../aliases";
       deps_test [ "amap.ml", ["Long__B"];
                   "user.ml", ["Amap"; "Long__A"];
                   "long__A.ml", [];
                   "long__B.ml", []
                 ]
     )
  &&
  ( Sys.chdir "../network";
  deps_test ["a.ml", ["B"; "Extern"]; "b.ml", []; "c.ml", ["A"] ]
  )
  &&
  ( Sys.chdir "../collision";
    deps_test ["a.ml", ["B"; "Ext"];
               "b.ml", [];
               "c.ml", ["B"];
               "d.ml", ["B"] ]
  )
  &&
  ( Sys.chdir "../pair";
  deps_test ["a.ml", ["B"];  "b.ml", ["Extern"] ]
  )
  && (
    let n = 100 in
    let dep = [ Printf.sprintf "M%d" n ] in
    Sys.chdir "../star";
    ignore @@ Sys.command (Printf.sprintf "ocaml generator.ml %d" 100);
    let rec deps k =
      if k >= n then
        [ Printf.sprintf "m%03d.mli" k, [] ]
      else
        (Printf.sprintf "m%03d.mli" k, dep) :: (deps @@ k+1) in
    deps_test @@ deps 1
  )
    &&
  ( Sys.chdir "../stops";
    deps_test ["a.ml", ["B"; "C"; "D"; "E"; "F"]
              ; "b.ml", ["Z"]
              ; "c.ml", ["Y"]
              ; "d.ml", ["X"]
              ; "e.ml", ["W"]
              ; "f.ml", ["V"]
              ; "v.ml", ["E"]
              ; "w.ml", ["D"]
              ; "x.ml", ["C"]
              ; "y.ml", ["B"]
              ; "z.ml", []
              ]
)
    && (
      Sys.chdir "..";
      cycle_test [["Self_cycle"]] ["self_cycle.ml"]
    )
    &&
    (
      Sys.chdir "ω-cycle";
      cycle_test [["C1";"C2";"C3";"C4";"C5"]] [ "a.ml"
                    ; "b.ml"
                    ; "c1.ml"
                    ; "c2.ml"
                    ; "c3.ml"
                    ; "c4.ml"
                    ; "c5.ml"
                    ; "k.ml"
                    ; "l.ml"
                    ; "w.ml"
                                              ]
    )
    && (
      Sys.chdir "../2+2-cycles";
      cycle_test [["A";"B"]; ["C";"D"]] [ "a.ml" ; "b.ml"; "c.ml"; "d.ml"]
    )
    &&
    ( Sys.chdir "../../lib";
      gen_deps_test (ocamlfind "compiler-libs") precise_deps_test
        [
          "ast_converter.mli", ( ["M2l"], ["Parsetree"], [] );
          "ast_converter.ml", ( ["M2l"; "Name"; "Option"; "Module";
                                 "Paths"; "Warning"],
                                ["List";"Longident"; "Location"; "Parsetree"], [] );
          "cmi.mli", (["M2l"], [], []);
          "cmi.ml", (["M2l";"Module"; "Option"; "Paths"],
                     ["Cmi_format";"Path";"Types"], []);
          "definition.mli", (["Module"], ["Format"], []);
          "definition.ml", (["Module"; "Name"; "Pp"], ["List"], []);
          "envts.mli", (["M2l";"Module";"Name"; "Interpreter"; "Paths"], [], []);
          "envts.ml", (
            ["Cmi"; "Definition"; "Interpreter"; "M2l"; "Module"; "Name";
             "Option"; "Paths"],
            ["Array"; "Filename"; "List";"Sys"],
            []);
          "error.mli", ([],["Format"],[]);
          "error.ml", ([],["Format"],[]);
          "interpreter.mli", (["Module";"Paths";"M2l"],[],[]);
          "interpreter.ml", (
            ["Definition"; "M2l"; "Module"; "Name"; "Option"; "Paths";
             "Result"; "Warning"]
          ,["List"],[]);
          "m2l.mli", (["Module";"Name";"Definition";"Paths" ],["Format"],[]);
          "m2l.ml", (["Module";"Name";"Option";"Definition";"Paths"; "Pp" ],
                     ["List"],[]);
          "module.mli", ( ["Paths";"Name"], ["Format"], [] );
          "module.ml", ( ["Error";"Paths";"Name"; "Pp" ], ["List"], [] );
          "name.mli", ( [], ["Format";"Set";"Map"], [] );
          "name.ml", ( ["Pp"], ["Set";"Map"], [] );
          "option.mli", ([],[],[]);
          "option.ml", ([],[],[]);
          "paths.mli", (["Name"], ["Map";"Set";"Format"],[]);
          "paths.ml", (["Name"; "Pp" ],
                       ["Filename";"List";"Map";"Set";"Format"; "String"],[]);
          "pp.mli", ([], ["Format"],[]);
          "pp.ml", ([], ["Format"],[]);
          "read.mli", (["M2l";"Name"],[],[]);
          "read.ml", (["Ast_converter"; "Error"; "M2l"],
                      ["Filename"; "Format"; "Location"; "Parse"; "Pparse";
                       "String"; "Syntaxerr"],[]);
          "result.mli", ([],[],[]);
          "result.ml", ([],["List"],[]);
          "solver.mli", (["Unit";"Name";"Interpreter"],["Format";"Map"],[]);
          "solver.ml", (
            ["Definition"; "Interpreter"; "M2l"; "Module"; "Name";
             "Option"; "Pp"; "Unit"],
            ["List"; "Map"],[]);
          "unit.mli", (["Paths"; "M2l"],["Format";"Set"],[]);
          "unit.ml", (
            ["M2l"; "Option"; "Paths"; "Pp"; "Read"],
            [ "List"; "Set"],
            []);
          "warning.mli", ([],["Format"],[]);
          "warning.ml", ([],["Format"],[]);
        ]
    )

let () =
  if result then
    Format.printf "Success.\n"
  else
    Format.printf "Failure.\n"
