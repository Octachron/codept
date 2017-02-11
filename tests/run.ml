module Pth = Paths.Pkg

let local = Pth.local

let (%) f g x = f (g x)

let classify filename =  match Filename.extension filename with
  | ".ml" -> { Read.format= Src; kind  = M2l.Structure }
  | ".mli" -> { Read.format = Src; kind = M2l.Signature }
  | _ -> raise (Invalid_argument "unknown extension")


let policy = Standard_policies.quiet

let read policy (info,f) =
  Unit.read_file policy info f

let organize policy files =
  files
  |> Unit.unimap (List.map @@ read policy)
  |> Unit.Groups.Unit.(fst % split % group)


module Envt = Envts.Tr

let start_env includes fileset =
  let base = Envts.Base.start @@ Module.Sig.flatten Stdlib.signature in
  let layered = Envts.Layered.create includes fileset base in
  let traced = Envts.Trl.extend layered in
  Envt.start traced fileset


module Branch(Param:Interpreter.param) = struct
  module S = Solver.Make(Envt)(Param)
  module D = Solver.Directed(Envt)(Param)

  let organize pkgs files =
    let units: _  Unit.pair = organize policy files in
    let fileset = units.mli
                  |> List.map (fun (u:Unit.s) -> u.name)
                  |> Name.Set.of_list in
    let env = start_env pkgs fileset in
    env, units

  let analyze_k pkgs files roots =
    let core, units = organize pkgs files in
    let gen = D.generator (read policy) (files.ml @ files.mli) in
    S.solve core units,
    Option.fmap (fun roots -> snd @@ D.solve gen core roots) roots

  module CSet = Set.Make(struct type t = string list let compare = compare end)

  let analyze_cycle files =
    let core, units = organize [] files in
    let rec solve_and_collect_cycles ancestors ?(learn=false) cycles state =
      match ancestors with
      | _ :: grand_father :: _ when grand_father = state -> core, cycles
      | _ ->
        match S.resolve_dependencies ~learn state with
        | Ok (e,_) -> e, cycles
        | Error state ->
          let units = state.pending in
          let module F = Solver.Failure in
          let _, cmap = F.analyze (S.alias_resolver state) units in
          let errs = F.Map.bindings cmap in
          let name unit = Solver.(unit.input.name) in
          let more_cycles =
            errs
            |> List.filter (function (F.Cycle _, _) -> true | _ -> false)
            |> List.map snd
            |> List.map (List.map name % Solver.Failure.Set.elements)
            |> CSet.of_list
          in
          solve_and_collect_cycles (state::ancestors) ~learn
            (CSet.union more_cycles cycles)
          @@ S.approx_and_try_harder state in
    let env, c =
      solve_and_collect_cycles []  ~learn:true CSet.empty
      @@ S.start core units.mli in
    snd @@ solve_and_collect_cycles [] c @@ S.start env units.ml



(*  let ok_only = function
    | Ok l -> l
    | Error (`Ml (_, state) | `Mli state)  ->
      Fault.Log.critical "%a" Solver.Failure.pp_cycle state.S.pending
*)

  let normalize set =
    set
    |> Deps.Forget.to_list
    |> List.map Pth.module_name
    |> List.sort compare


  let simple_dep_test name list set =
    let r = normalize set = List.sort compare list in
    if not r then
      Pp.p "Failure %a(%s): expected:[%a], got:@[[%a]@]\n"
        Pth.pp name (Sys.getcwd ())
        Pp.(list estring) (List.sort compare list)
        Pp.(list estring) (normalize set);
    r


  let (%) f g x = f (g x)

  let normalize_2 set =
    let l = Deps.Forget.to_list set in
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

  let add_info {Unit.ml; mli} (f, info) =
    let k = classify f in
    match k.kind with
    | M2l.Structure -> { Unit.ml = (k,f,info) :: ml; mli }
    | M2l.Signature -> { Unit.mli = (k,f,info) :: mli; ml }

  let add_file {Unit.ml; mli} file =
    let k = classify file in
    match k.kind with
    | M2l.Structure -> { Unit.ml = (k,file) :: ml; mli }
    | M2l.Signature -> { Unit.mli = (k,file) :: mli; ml }

  let gen_deps_test libs inner_test roots l =
    let {Unit.ml;mli} = List.fold_left add_info {Unit.ml=[]; mli=[]} l in
    let module M = Paths.S.Map in
    let build exp = List.fold_left (fun m (_,x,l) ->
        M.add (Paths.S.parse_filename x) l m)
        M.empty exp in
    let exp = M.union' (build ml) (build mli) in
    let sel (k,f,_) = k, f in
    let files = Unit.unimap (List.map sel) {ml;mli} in
    let (u, u' : _ Unit.pair * _ )  = analyze_k libs files roots in
    let (=?) expect files = List.for_all (fun u ->
        let path = u.Unit.path.Pth.file in
        let expected =
          Paths.S.Map.find path expect
        in
        inner_test u.path expected u.dependencies
      ) files in
    exp =? u.ml && exp =? u.mli && Option.( u' >>| (=?) exp >< true )

  let deps_test (roots,l) =
    gen_deps_test [] simple_dep_test roots l

  let deps_test_single l = deps_test (None,l)

  let ocamlfind name =
    let cmd = "ocamlfind query " ^ name in
    let cin = Unix.open_process_in cmd in
    try
      [input_line cin]
    with
      End_of_file -> []

  let cycle_test expected l =
    let files = List.fold_left add_file {Unit.ml=[]; mli=[]} l in
    let cycles = analyze_cycle files in
      let expected = List.map (List.sort compare) expected in
      let cycles = List.map (List.sort compare) (CSet.elements cycles) in
      let r = cycles = expected in
      if not r then
        ( Pp.fp Pp.std "Failure: expected %a, got %a\n"
            Pp.(list @@ list string) expected
            Pp.(list @@ list string) cycles;
          r )
      else
        r
end

module Std = Branch(struct
  let policy = policy
  let epsilon_dependencies = false
  let transparent_aliases = true
  let transparent_extension_nodes = true
  end)

module Eps = Branch(struct
  let policy = policy
  let epsilon_dependencies = true
  let transparent_aliases = true
  let transparent_extension_nodes = true
  end)

let both root x =
  Std.deps_test (Some root, x) && Eps.deps_test (Some root, x)

let result =
  Sys.chdir "tests/cases";
  List.for_all Std.deps_test_single [
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
    ["imperfect_modules.ml", ["Ext"] ];
    ["include.ml", ["List"] ];
    ["include_functor.ml", ["A"] ];
    ["letin.ml", ["List"] ];
    ["let_open.ml", [] ];
    ["module_rec.ml", ["Set"] ];
    ["module_types.ml", ["B"] ];
    ["more_functor.ml", ["Ext";"Ext2"] ];
    ["nested_modules.ml", [] ];
    ["no_deps.ml", [] ];
    ["not_self_cycle.ml", ["E"] ];
    ["nothing.ml", [] ];
    ["unknown_arg.ml", ["Ext"] ];
    ["opens.ml", ["A";"B"] ];
    ["pattern_open.ml", ["A'";"E1"; "E2"; "E3";"E4"] ];
    ["phantom_maze.ml", ["External"; "B"; "C";"D"; "E"; "X"; "Y" ] ];
    ["phantom_maze_2.ml", ["E"; "D"]];
    ["phantom_maze_3.ml", ["B"; "X"] ];
    ["phantom_maze_4.ml", ["Extern"] ];
    ["recmods.ml", ["Ext"]];
    ["record.ml", ["Ext";"E2";"E3"]];
    ["simple.ml", ["G";"E"; "I"; "A"; "W"; "B"; "C"; "Y"; "Ext"]];
    ["solvable.ml", ["Extern"]];
    ["tuple.ml", ["A"; "B"; "C"]];
    ["unknown_arg.ml", ["Ext"] ];
    ["with.ml", ["Ext"] ]


  ]
  (* Note the inferred dependencies is wrong, but there is not much
     (or far too much ) to do here *)
  && Std.deps_test_single  [ "riddle.ml", ["M5"] ]
  &&
  List.for_all Std.deps_test_single [
    ["broken.ml", ["Ext"; "Ext2"; "Ext3"; "Ext4"; "Ext5" ] ];
    ["broken2.ml", ["A"; "Ext"; "Ext2" ]];
      ["broken3.ml", []];
  ]
  &&( Sys.chdir "../mixed";
      both ["A"] ["a.ml", ["D"];
                 "a.mli", ["D";"B"];
                 "b.ml", ["C"];
                 "c.mli", ["D"];
                 "d.mli", []
                ]
    )
  && ( Sys.chdir "../aliases";
       both ["User"] [ "amap.ml", ["Long__B"];
                   "user.ml", ["Amap"; "Long__A"];
                   "long__A.ml", [];
                   "long__B.ml", []
                 ]
     )
  && ( Sys.chdir "../aliases2";
       both ["A";"C";"E"] [ "a.ml", ["B"; "D" ];
                   "b.ml", [];
                   "c.ml", [];
                   "d.ml", [];
                   "e.ml", []
                 ]
     )
  && (Sys.chdir "../aliases_and_map";
      both ["n__C"; "n__D"] ["n__A.ml", ["M"];
                 "n__B.ml", ["M"];
                 "n__C.ml", ["M"; "N__A"];
                 "n__D.ml", ["M"; "N__B"];
                 "m.ml", [];
                 "n__A.mli", ["M"];
                 "n__B.mli", ["M"];
                 "n__C.mli", ["M"];
                 "n__D.mli", ["M"];
                 "m.mli", [];
                ]
     )
  &&
  ( Sys.chdir "../broken_network";
    both ["A"] [
      "a.ml", ["Broken"; "B"; "C"];
      "broken.ml", ["B"];
      "c.ml", ["Extern"] ]
  )
  &&
  ( Sys.chdir "../network";
  both ["C"] ["a.ml", ["B"; "Extern"]; "b.ml", []; "c.ml", ["A"] ]
  )
  &&
  ( Sys.chdir "../collision";
    both ["A";"C";"D"]
      ["a.ml", ["B"; "Ext"];
       "b.ml", [];
       "c.ml", ["B"];
       "d.ml", ["B"] ]
  )
  &&
  ( Sys.chdir "../pair";
  both ["A"] ["a.ml", ["B"];  "b.ml", ["Extern"] ]
  )
  &&
  ( Sys.chdir "../namespaced";
    both ["NAME__a"; "NAME__c"] [ "NAME__a.ml", ["Main"; "NAME__b"];
                "NAME__b.ml", ["Main"; "NAME__c"];
                "NAME__c.ml", ["Main"];
                "main.ml", []
              ]
  )
  &&
  ( Sys.chdir "../module_types";
  both ["B"] ["a.mli", ["E"];  "b.ml", ["A"] ]
  )
  &&
  (
    Sys.chdir "../5624";
    Eps.deps_test
      (Some ["C"],
       [ "a.mli", [];
         "b.mli", ["A"];
         "c.ml", ["A"; "B"]
       ]
      )
  )
  &&
  (
    Sys.chdir "../deep-eps";
    Eps.deps_test
      (Some ["C"] ,
       [
         "a.mli", ["W";"Z"; "K"];
         "b.mli", ["A"; "W"; "Z"];
         "c.ml", ["A"; "B"; "W"; "Z"];
         "k.ml", ["W"; "Z"];
         "y.mli", [];
         "w.mli", ["Y"];
         "z.mli", ["W"]
       ]
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
    both ["M001"] @@ deps 1
  )
    &&
  ( Sys.chdir "../stops";
    both ["A"] ["a.ml", ["B"; "C"; "D"; "E"; "F"]
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
      Sys.chdir "../cases";
      Std.cycle_test [["Self_cycle"]] ["self_cycle.ml"]
    )
    &&
    (
      Sys.chdir "../ω-cycle";
      Std.cycle_test [["C1";"C2";"C3";"C4";"C5"]] [ "a.ml"
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
      Std.cycle_test [["A";"B"]; ["C";"D"]] [ "a.ml" ; "b.ml"; "c.ml"; "d.ml"]
    )
    && (
      Sys.chdir "../8-cycles";
      Std.cycle_test [ ["A";"B";"C";"D"]; ["A"; "H"; "G"; "F"; "E"]]
        [ "a.ml" ; "b.ml"; "c.ml"; "d.ml"; "e.ml"; "f.ml"; "g.ml"; "h.ml"]
    )
    && (
      Sys.chdir "../aliased_cycle";
      Std.cycle_test [ ["A"; "B"; "C"]]
        [ "a.ml" ; "b.ml"; "c.ml"; "atlas.ml"]
    )
    &&
    ( Sys.chdir "../../lib";
      Std.gen_deps_test (Std.ocamlfind "compiler-libs") Std.precise_deps_test
        (Some ["Solver"; "Standard_policies"])
        [
          "ast_converter.mli", ( ["M2l"], ["Parsetree"], [] );
          "ast_converter.ml", ( ["Loc"; "M2l"; "Name"; "Option"; "Module";
                                 "Paths"],
                                ["List";"Longident"; "Location"; "Lexing";
                                 "Parsetree"], [] );
          "approx_parser.mli", (["M2l"], [],[]);
          "approx_parser.ml", (["Deps"; "Loc"; "Read";"M2l";"Name"],
                               ["Lexer"; "Parser"; "Lexing";"List"],[]);
          "cmi.mli", (["M2l"], [], []);

          "cmi.ml", (["Loc"; "M2l";"Module"; "Option"; "Paths"],
                     ["Cmi_format"; "List"; "Path";"Types"], []);
          "deps.ml", (["Option"; "Paths"; "Pp"; "Sexp"],["List"],[]);
          "summary.mli", (["Module"], ["Format"], []);
          "summary.ml", (["Module"; "Name"; "Pp"; "Mresult"], ["List"], []);
          "envts.mli", (["Deps"; "Module";"Name"; "Interpreter"; "Paths"], [], []);
          "envts.ml", (
            ["Cmi"; "Deps"; "Summary"; "Interpreter"; "M2l"; "Fault";
             "Module"; "Name"; "Paths";"Standard_faults";"Standard_policies"],
            ["Array"; "Filename"; "List";"Sys"],
            []);
          "interpreter.mli", (["Deps"; "Fault"; "Module"; "Name"; "Paths";"M2l";
                               "Summary"],
                              [],[]);
          "interpreter.ml", (
            ["Summary"; "Loc"; "M2l"; "Module"; "Name"; "Option"; "Paths";
             "Mresult"; "Fault"; "Standard_faults"; "Deps"]
          ,["List"],[]);
          "m2l.mli", (["Deps";"Loc"; "Module";"Name";"Summary";"Paths";"Sexp" ],
                      ["Format"],[]);
          "m2l.ml", (["Loc"; "Deps"; "Module"; "Mresult"; "Name";
                      "Option";"Summary";"Paths"; "Pp"; "Sexp" ],
                     ["List"],[]);
          "fault.ml", (["Loc"; "Option"; "Name";"Paths"; "Pp"],
                          ["Array"; "Format"],[]);
          "fault.mli", (["Loc"; "Paths"; "Name"],
                          ["Format"],[]);

          "module.mli", ( ["Loc";"Paths";"Name"; "Sexp"], ["Format"], [] );
          "module.ml", ( ["Loc";"Paths";"Name"; "Pp"; "Sexp" ], ["List"], [] );
          "name.mli", ( [], ["Format";"Set";"Map"], [] );
          "name.ml", ( ["Pp"], ["Set";"Map"], [] );
          "loc.mli", ( ["Sexp"], ["Format"], []);
          "loc.ml", ( ["Pp";"Sexp"], ["List"], []);
          "option.mli", ([],[],[]);
          "option.ml", ([],["List"],[]);
          "paths.mli", (["Name"; "Sexp"], ["Map";"Set";"Format"],[]);
          "paths.ml", (["Name"; "Pp"; "Sexp" ],
                       ["Filename";"List";"Map";"Set";"Format"; "String"],[]);
          "pp.mli", ([], ["Format"],[]);
          "pp.ml", ([], ["Format"],[]);
          "read.mli", (["M2l"; "Name"],["Syntaxerr"],[]);
          "read.ml", (["Ast_converter"; "Cmi"; "M2l"],
                      ["Filename"; "Format"; "Lexing"; "Location"; "Parse";
                       "Parsing"; "Pparse"; "String"; "Syntaxerr"],
                      ["Sexp_parse"; "Sexp_lex"]);
          "mresult.mli", ([],[],[]);
          "mresult.ml", ([],["List"],[]);
          "sexp.ml", (["Name"; "Option"; "Pp"],
                      [ "Obj"; "Format";"List";"Map"; "Hashtbl"; "String"], [] );
          "sexp.mli", (["Name"],
                      [ "Format"], [] );
          "solver.mli", (["Deps"; "Fault"; "Loc"; "Unit";"M2l";"Name";"Read";
                          "Summary"; "Interpreter"; "Paths"],
                         ["Format";"Map";"Set"],[]);
          "solver.ml", (
            ["Approx_parser"; "Deps"; "Summary"; "Interpreter"; "Loc";
             "M2l"; "Module"; "Mresult"; "Name"; "Option"; "Pp"; "Paths"; "Read";
             "Unit"; "Fault"; "Standard_faults"],
            ["List"; "Map"; "Set"],[]);
          "standard_faults.ml", (["Fault"; "Module"; "Paths"; "Pp"; "Loc" ],
                                 ["Format"; "Location"; "Syntaxerr"],[]);
          "standard_faults.mli", (["Fault"; "Name"; "Module"; "Paths" ],
                                  ["Syntaxerr"],[]);
          "standard_policies.ml", (["Fault"; "Standard_faults"; "Solver"],[],[]);
          "standard_policies.mli", (["Fault"],[],[]);
          "unit.mli", (["Deps";"Paths"; "M2l"; "Module"; "Name"; "Fault"; "Read"],
                       ["Format";"Set"],[]);
          "unit.ml", (
            ["Approx_parser"; "Deps"; "M2l"; "Module"; "Fault";
             "Option"; "Paths"; "Pp"; "Read"; "Standard_faults"],
            [ "List"; "Set"],
            []);
        ]
      )
    )

let () =
  if result then
    Format.printf "Success.\n"
  else
    Format.printf "Failure.\n"
