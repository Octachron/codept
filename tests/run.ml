module Pth = Paths.Pkg

let local = Pth.local

let root = if Array.length Sys.argv > 0 then Some (Sys.argv.(1)) else None

let (%) f g x = f (g x)
let (~:) = List.map Namespaced.make

let classify filename =  match Support.extension filename with
  | "ml" -> { Read.format= Src; kind  = M2l.Structure }
  | "mli" -> { Read.format = Src; kind = M2l.Signature }
  | ext -> raise (Invalid_argument ("unknown extension: "^ext))

module Version = struct
  let major, minor = Scanf.sscanf Sys.ocaml_version "%d.%d" (fun x y -> x, y)
  type t = {major:int; minor:int}
  let v = { major; minor }

  let v_4_04 = { minor = 4; major = 4 }
  let v_4_06 = { minor = 6; major = 4 }
  let v_4_07 = { minor = 7; major = 4 }
  let v_4_08 = { minor = 8; major = 4 }

end


let policy = Standard_policies.quiet

let read policy (info,f,path) =
  Unit.read_file policy info f path

let read_simple policy (info,f) =
  read policy (info, f, Namespaced.of_filename f)

let organize policy files =
  files
  |> Unit.unimap (List.map @@ read policy)
  |> Unit.Group.(fst % split % group)

let version = Sys.ocaml_version

let start_env includes files =
  Envt.start ~open_approximation:true
    ~libs:includes
    ~namespace:files
    ~implicits:[["Stdlib"], Bundle.stdlib ]
    Module.Dict.empty

module Branch(Param:Stage.param) = struct

  (*  module Engine=Outliner.Make(Envt.Core)(Param)*)
  module Engine = Dep_zipper.Make(Envt.Core)(Param)
  module S = Solver.Make(Envt.Core)(Param)(Engine)
  module D = Solver.Directed(Envt.Core)(Param)(Engine)

  let organize pkgs files =
    let units: _  Unit.pair = organize policy files in
    let fileset = List.map (fun (u:Unit.s) -> u.path) units.mli in
    let env = start_env pkgs fileset in
    env, units

  let analyze_k pkgs files roots =
    let core, units = organize pkgs files in
    let loader, files = read policy, files.ml @ files.mli in
    S.solve core units,
    Option.fmap (fun roots -> snd @@ D.solve loader files core roots) roots

  module CSet =
    Set.Make(struct
      type t = Name.t list
      let compare = compare
    end)

  let analyze_cycle files =
    let core, units = organize [] files in
    let rec solve_and_collect_cycles
        ancestors ?(learn=false) cycles state =
      match ancestors with
      | _ :: grand_father :: _ when S.eq grand_father state ->
        core, cycles
      | _ ->
        match S.resolve_dependencies ~learn state with
        | Ok (e,_) -> e, cycles
        | Error state ->
          let units = state.pending in
          let module F = Solver.Failure in
          let _, cmap = F.analyze S.blocker (S.alias_resolver state) units in
          let errs = F.to_list cmap in
          let name input = input.Unit.path in
          let more_cycles =
            errs
            |> List.filter (function (F.Cycle _, _) -> true | _ -> false)
            |> List.map snd
            |> List.map (List.map name)
            |> (* FIXME*)
            List.map (List.map (fun x -> x.Namespaced.name))
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
    |> Deps.pkgs
    |> List.sort compare


  let simple_dep_test name list set =
    let r = normalize set = List.sort compare list in
    if not r then
      Pp.p "Failure %a(%s): expected:[%a], got:@[[%a]@]\n"
        Pth.pp name (Sys.getcwd ())
        Pp.(list Paths.P.pp) (List.sort compare list)
        Pp.(list Paths.P.pp) (normalize set);
    r

  let (%) f g x = f (g x)

  let normalize_2 set =
    let l = Deps.pkgs set in
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
    && test "unknown" unkw unkw'

  let add_info {Unit.ml; mli} ((f,p), info) =
    let k = classify f in
    let x = (k,f,p, info) in
    match k.kind with
    | M2l.Structure -> { Unit.ml = x :: ml; mli }
    | M2l.Signature -> { Unit.mli = x :: mli; ml }

  let add_file {Unit.ml; mli} file =
    let k = classify file in
    let x = k, file, Namespaced.of_filename file in
    match k.kind with
    | M2l.Structure -> { Unit.ml = x :: ml; mli }
    | M2l.Signature -> { Unit.mli = x :: mli; ml }

  let gen_deps_test libs inner_test roots l =
    let {Unit.ml;mli} =
      List.fold_left add_info {Unit.ml=[]; mli=[]} l in
    let module M = Paths.P.Map in
    let build exp = List.fold_left (fun m (_,f,_n,l) ->
        M.add (Paths.P.local f) l m)
        M.empty exp in
    let exp = M.union (fun _ x _ -> Some x) (build ml) (build mli) in
    let sel (k,f,n, _) = k, f, n in
    let files = Unit.unimap (List.map sel) {Unit.ml;mli} in
    let (u, u' : _ Unit.pair * _ )  =
      analyze_k libs files roots in
    let (=?) expect files = List.for_all (fun u ->
        let path = u.Unit.src in
        let expected =
          M.find path expect
        in
        inner_test u.src expected (Unit.deps u)
      ) files in
    exp =? u.ml && exp =? u.mli && Option.( u' >>| (=?) exp >< true )

  let deps_test (roots,l) =
    gen_deps_test [] simple_dep_test
      (Option.fmap (~:) roots) l

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

let l = List.map Paths.P.local
let u = List.map (fun x -> Paths.P.{(local x) with source=Unknown})

let std =
  let localize x = local x in
  List.map
    (fun x -> Paths.P.{(localize x) with source=Special "stdlib"})

let d x =
  x,
  let nms = List.map String.capitalize_ascii @@ Support.split_on_char '/' x in
  let n = Namespaced.of_path nms in
  let name = Paths.S.(module_name @@ parse_filename n.name) in
  { n with name }

let (/) p x =
  x, Namespaced.of_filename ~nms:[p] x

let dl x = List.map (fun (f,deps) -> d f, deps) x

let chdir x = Sys.chdir x
let () = match root with None -> () | Some root -> chdir root


let result =
  chdir "tests/cases";
  List.for_all Std.deps_test_single @@ List.map dl [
    ["abstract_module_type.ml", []];
    ["alias_map.ml", u["Aliased__B"; "Aliased__C"] ];
    ["apply.ml", u["F"; "X"]];
    ["basic.ml", u["Ext"; "Ext2"]];
    ["bindings.ml", []];
    ["bug.ml", std ["Sys"] ];
    ["case.ml", u["A"; "B";"C";"D";"F"]];
    ["destructive_substitutions.ml", u["Ext"]];
    ["even_more_functor.ml", u["E"; "A"]];
    ["first-class-modules.ml", u["Mark";"B"] ];
    ["first_class_more.ml", [] ];
    ["first_class_encore.ml", u["Ext"] ];
    ["foreign_arg_sig.ml", u["Ext";"Ext2"] ];
    ["functor.ml", [] ];
    ["functor_in_type_expr.ml", u["Ext1";"Ext2";"Ext3";"Ext4"] ];
    ["functor_with_include.ml", [] ];
    ["hidden.ml", u["Ext"] ];
    ["imperfect_modules.ml", u["Ext"] ];
    ["include.ml", std ["List"] ];
    ["include_functor.ml", u["A"] ];
    ["letin.ml", std ["List"] ];
    ["let_open.ml", [] ];
    ["module_rec.ml", std ["Set"] ];
    ["module_types.ml", u["B"] ];
    ["more_functor.ml", u["Ext";"Ext2"] ];
    ["nested_modules.ml", [] ];
    ["no_deps.ml", [] ];
    ["not_self_cycle.ml", u["E"] ];
    ["nothing.ml", [] ];
    ["unknown_arg.ml", u["Ext"] ];
    ["opens.ml", u["A";"B"] ];
    ["phantom_maze.ml", u["External"; "B"; "C";"D"; "E"; "X"; "Y" ] ];
    ["phantom_maze_2.ml", u["E"; "D"]];
    ["phantom_maze_3.ml", u["B"; "X"] ];
    ["phantom_maze_4.ml", u["Extern"] ];
    ["recmods.ml", u["Ext";"Xeno";"Xeno2";"Xeno3"]];
    ["record.ml", u["Ext";"E2";"E3"]];
    ["simple.ml", u["G";"E"; "I"; "A"; "W"; "B"; "C"; "Y"; "Ext"]];
    ["solvable.ml", u["Extern"]];
    ["tuple.ml", u["A"; "B"; "C"]];
    ["unknown_arg.ml", u["Ext"] ];
    ["with.ml", u["Ext"]];
    ["with_more.ml", u["Ext";"Ext3"]]
  ]

  && ( Version.( v < v_4_04) ||
       Std.deps_test_single [d"pattern_open.ml", u["A'";"E1"; "E2"; "E3";"E4"]] )

  && ( Version.( v < v_4_08) ||
       Std.deps_test_single [d"option_monad.ml", u["Ext";"Ext3";"Ext4"]]
     )

  (* Note the inferred dependencies is wrong, but there is not much
     (or far too much ) to do here *)
  && Std.deps_test_single  [ d"riddle.ml", u["M5"] ]
  &&
  List.for_all Std.deps_test_single @@ List.map dl [
    ["broken.ml", u["Ext"; "Ext2"; "Ext3"; "Ext4"; "Ext5" ] ];
    ["broken2.ml", u["A"; "Ext"; "Ext2" ]];
    ["broken3.ml", []];
  ]
  &&( chdir "../complex/mixed";
      both ["A"] @@ dl
        ["a.ml", l["d.mli"];
         "a.mli", l["b.ml"; "d.mli"];
         "b.ml", l["c.mli"];
         "c.mli", l["d.mli"];
         "d.mli", []
        ]
    )
  && ( chdir "../aliases";
       both ["User"] @@ dl
         [ "amap.ml", l["long__B.ml"];
           "user.ml", l["amap.ml"; "long__A.ml"];
           "long__A.ml", [];
           "long__B.ml", []
         ]
     )
  && ( chdir "../aliases2";
       both ["A";"C";"E"] @@ dl
         [ "a.ml", l["b.ml"; "d.ml" ];
           "b.ml", [];
           "c.ml", [];
           "d.ml", [];
           "e.ml", []
         ]
     )
  && (chdir "../aliases_and_map";
      both ["n__C"; "n__D"] @@ dl
        ["n__A.ml", l["m.ml"];
         "n__B.ml", l["m.ml"];
         "n__C.ml", l["m.ml"; "n__A.ml"];
         "n__D.ml", l["m.ml"; "n__B.ml"];
         "m.ml", [];
         "n__A.mli", l["m.mli"];
         "n__B.mli", l["m.mli"];
         "n__C.mli", l["m.mli"];
         "n__D.mli", l["m.mli"];
         "m.mli", [];
        ]
     )

  && (chdir "../alias_values";
      both ["C"] @@ dl
        ["a.ml", l[];
         "b.ml", l[];
         "c.ml", l["a.ml"; "b.ml"]
        ]
     )
  &&
  ( chdir "../broken_network";
    both ["A"] @@ dl [
      "a.ml", (l["broken.ml"; "c.ml"] @ u["B"]);
      "broken.ml", u["B"];
      "c.ml", u["Extern"] ]
  )
  &&
  ( chdir "../network";
    both ["C"] @@ dl
      ["a.ml", l["b.ml"]@ u ["Extern"];
       "b.ml", []; "c.ml", l["a.ml"] ]
  )
  &&
  ( chdir "../collision";
    both ["A";"C";"D"] @@ dl
      ["a.ml", l["b.ml"] @ u ["Ext"];
       "b.ml", [];
       "c.ml", l["b.ml"];
       "d.ml", l["b.ml"] ]
  )
  &&
  ( chdir "../pair";
  both ["A"] [d"a.ml", l["b.ml"];  d"b.ml", u["Extern"] ]
  )
  &&
  ( chdir "../namespaced";
    both ["NAME__a"; "NAME__c"] @@ dl [
      "NAME__a.ml", l["main.ml"; "NAME__b.ml"];
      "NAME__b.ml", l["main.ml"; "NAME__c.ml"];
      "NAME__c.ml", l["main.ml"];
      "main.ml", []
    ]
  )
  &&
  ( chdir "../module_types";
  both ["B"] @@ dl ["a.mli", u["E"]; "b.ml", l["a.mli"] ]
  )
  &&
  (
    chdir "../5624";
    Eps.deps_test
      (Some ["C"],
       [ d"a.mli", [];
         d"b.mli", l["a.mli"];
         d"c.ml", l["a.mli"; "b.mli"]
       ]
      )
  )
  &&
  (
    chdir "../deep-eps";
    Eps.deps_test
      (Some ["C"] ,
       dl [
         "a.mli", l["w.mli";"z.mli"; "k.ml"];
         "b.mli", l["a.mli"; "w.mli"; "z.mli"];
         "c.ml", l["a.mli"; "b.mli"; "w.mli"; "z.mli"];
         "k.ml", l["w.mli"; "z.mli"];
         "y.mli", [];
         "w.mli", l["y.mli"];
         "z.mli", l["w.mli"]
       ]
  )
  && (
    let n = 100 in
    let dep = l[ Printf.sprintf "m%d.mli" n ] in
    chdir "../star";
    ignore @@ Sys.command (Printf.sprintf "ocaml generator.ml %d" 100);
    let rec deps k =
      if k >= n then
        [ Printf.sprintf "m%03d.mli" k, [] ]
      else
        (Printf.sprintf "m%03d.mli" k, dep) :: (deps @@ k+1) in
    both ["M001"] @@ dl @@ deps 1
  )
    &&
  ( chdir "../stops";
    both ["A"] @@ dl
      [ "a.ml", l["b.ml"; "c.ml"; "d.ml"; "e.ml"; "f.ml"]
      ; "b.ml", l["z.ml"]
      ; "c.ml", l["y.ml"]
      ; "d.ml", l["x.ml"]
      ; "e.ml", l["w.ml"]
      ; "f.ml", l["v.ml"]
      ; "v.ml", l["e.ml"]
      ; "w.ml", l["d.ml"]
      ; "x.ml", l["c.ml"]
      ; "y.ml", l["b.ml"]
      ; "z.ml", l[]
      ]
  )

    (*Namespace related tests *)
    && ( chdir "../uncoupled";
         both ["A";"C"]
           [
             d"a.ml", l["b.ml"];
            "N" / "b.ml", [];
            d"c.ml", l ["b.ml"]
           ]
       )

    && begin chdir "../any_m";
        both ["M.F"; "M.D"] @@ dl
        [
          "a/m.ml", l["b/m.ml"];
          "b/m.ml", l["d/m.ml";"e/m.ml"];
          "c/m.ml", l["d/m.ml"] @ u["Ext"];
          "d/m.ml", [];
          "e/m.ml", l["d/m.ml"];
          "f/m.ml", l["a/m.ml"]
        ]
    end

    && begin chdir "../siblings";
        both ["L.L.A"; "A"; "L.A"; "R.R.A"; "R.R.B" ] @@ dl
        [
          "L/L/a.ml", l["L/L/b.ml"; "L/R/a.ml"];
          "L/L/b.ml", l["L/R/b.ml"];
          "L/a.ml", l["L/L/a.ml";"L/R/a.ml"];
          "L/R/a.ml", l["L/R/b.ml"];
          "L/R/b.ml", [];
          "a.ml", l["L/L/a.ml";"R/R/b.ml"];
          "R/R/a.ml", l["R/R/b.ml"; "R/L/a.ml"];
          "R/R/b.ml", l["R/L/b.ml"];
          "R/L/a.ml", l["R/L/b.ml"];
          "R/L/b.ml", []

        ]
    end
    && begin chdir "../alias_path_with_namespaces";
      both ["Inner.B"] @@ dl
        [
          "inner/a.mli", [];
          "inner/a.ml", l["inner/b.mli"] @ u["C"];
          "inner/b.mli", []
        ]
    end

    (* Cycle tests *)
    && (
      chdir "../../cases";
      Std.cycle_test [["Self_cycle"]] ["self_cycle.ml"]
    )
    &&
    (
      chdir "../complex/ω-cycle";
      Std.cycle_test [["C1";"C2";"C3";"C4";"C5"]] [
        "a.ml"
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
      chdir "../2+2-cycles";
      Std.cycle_test [["A";"B"]; ["C";"D"]] [ "a.ml" ; "b.ml"; "c.ml"; "d.ml"]
    )
    && (
      chdir "../8-cycles";
      Std.cycle_test [ ["A";"B";"C";"D"]; ["A"; "H"; "G"; "F"; "E"]]
        [ "a.ml" ; "b.ml"; "c.ml"; "d.ml"; "e.ml"; "f.ml"; "g.ml"; "h.ml"]
    )
    && (
      chdir "../aliased_cycle";
      Std.cycle_test [ ["A"; "B"; "C"]]
        [ "a.ml" ; "b.ml"; "c.ml"; "atlas.ml"]
    )
    && ( Version.( v < v_4_08) || (
         chdir "../local_module_subst";
         both ["A";"B";"E"] @@ dl [
           "a.mli", [];
           "b.mli", l["a.mli"] @ u["Local"] ;
         ])
       )
    &&
    ( chdir "../../../lib";
      Std.gen_deps_test (Std.ocamlfind "compiler-libs") Std.precise_deps_test
        (Some ~:["Solver"; "Standard_policies"])
        (dl[
          "ast_converter.mli", ( ["M2l"], ["Parsetree"], [] );
          "approx_parser.mli", (["M2l"], [],[]);
          "approx_parser.ml", (["Deps"; "Loc"; "Read";"M2l";"Paths"],
                               ["Lexer"; "Parser";"Lexing";"List"],[]);
          "cmi.mli", (["M2l"], [], []);

          "deps.ml", (["Option"; "Paths"; "Pp"; "Schematic"],["List"],[]);
          "summary.mli", (
            ["Module";"Paths";"Schematic"; "Pp"],
            ["Format"],
            [] );
          "summary.ml", (
            ["Module"; "Pp"; "Mresult"; "Name"; "Schematic"],
            ["List"], []);
          "envt.mli", (
            ["Fault"; "Module";"Name"; "Namespaced"; "Stage"; "Paths";
             "Transforms"],
            [], []);
          "envt.ml", (
            ["Cmi"; "Deps"; "Summary"; "Transforms";
             "Fault"; "Dep_zipper"; "Module"; "Name"; "Namespaced"; "Paths";
             "Standard_faults";"Standard_policies";"Option";"Pp"],
            ["Array"; "Filename";"List";"Sys"; "Format"],
            []);
          "m2l.mli", (["Deps";"Loc"; "Module";"Name";"Paths"; "Pp"; "Schematic" ],
                      ["Format"],[]);
          "m2l.ml", (["Loc"; "Deps"; "Module"; "Name";
                      "Option";"Paths"; "Pp"; "Schematic"; "Schematic_indices" ],
                     ["List"],[]);
          "fault.ml", (["Format_tags"; "Loc"; "Option"; "Name";"Paths"; "Pp"],
                          ["Array"; "Format"; "Map"],[]);
          "fault.mli", (["Loc"; "Paths"], ["Format"],[]);
          "format_tags.mli", ([],["Format"],[]);
          "format_compat.mli", ([],["Format"],[]);
          "module.mli", ( ["Loc";"Paths";"Name";"Namespaced"; "Schematic"],
                          ["Format"], [] );
          "module.ml", ( ["Loc";"Paths";"Name"; "Namespaced"; "Option"; "Pp"
                         ; "Schematic"; "Schematic_indices" ],
                         ["List"], [] );
          "name.mli", ( [], ["Format";"Set";"Map"], [] );
          "name.ml", ( ["Pp"], ["Set";"Map";"List"], [] );
          "namespaced.mli", ( ["Name";"Paths"; "Pp"],
                              ["Set";"Map"], [] );
          "namespaced.ml", ( ["Name"; "Paths"; "Pp"],
                             ["Format";"List";"Set";"Map"], [] );
          "loc.mli", ( ["Schematic"], ["Format"], []);
          "loc.ml", ( ["Pp";"Schematic"], ["List"], []);
          "option.mli", ([],["Lazy"],[]);
          "option.ml", ([],["List";"Lazy"],[]);
          "pparse_compat.mli", ([], ["Parsetree"], []);
          "paths.mli", (["Name"; "Pp"; "Schematic"], ["Map";"Set";"Format"],[]);
          "paths.ml", (["Name"; "Pp"; "Schematic"; "Schematic_indices"; "Support" ],
                       ["Filename";"List";"Map";"Set";"Format";
                        "String"],[]);
          "pp.mli", ([], ["Format"],[]);
          "pp.ml", ([], ["Format"],[]);
          "read.mli", (["M2l"; "Name"; "Schematic"],["Lexer";"Syntaxerr"],[]);
          "read.ml", (["Ast_converter"; "Cmi"; "M2l"; "Pparse_compat";"Schema"
                      ; "Schematic" ],
                      ["Filename"; "Lexer"; "Lexing"; "List"; "Location";
                       "Parse"; "Parsetree"; "Parsing"; "Pparse"; "String";
                       "Syntaxerr"],
                      ["Sparser";"Slex"]);
          "mresult.mli", ([],[],[]);
          "mresult.ml", ([],["List"],[]);
          "schema.ml", (["M2l"; "Module"; "Option"; "Paths"; "Schematic"],
                        [],  []);
          "schema.mli", (["M2l"; "Module"; "Paths"; "Schematic"], [], []);
          "schematic.mli", ([],["Format"], []);
          "schematic_indices.ml", (["Schematic"],[],[]);

          "schematic.ml", (["Format_compat"; "Name"; "Mresult"; "Pp";
                            "Support";"Option"],
                           ["Format"; "Hashtbl"; "List"; "Map"; "String"],
                           [] );
          "solver.mli", (["Fault"; "Loc"; "Unit";
                          "Namespaced"; "Read";
                          "Summary"; "Stage"; "Paths"],
                         ["Format"],[]);
          "solver.ml", (
            ["Approx_parser"; "Deps"; "Summary"; "Stage"; "Loc";
             "M2l"; "Module"; "Mresult"; "Namespaced";
             "Option"; "Pp"; "Paths"; "Read"; "Unit"; "Fault";
             "Standard_faults"],
            ["List"; "Map";"Format"],[]);
          "standard_faults.ml", (
            ["Fault"; "Format_tags"; "Module"; "Namespaced"; "Paths"; "Pp"
            ; "Loc"; "Schematic" ],
            ["Format"; "Location"; "Syntaxerr"],[]);
          "standard_faults.mli", (
            ["Fault"; "Name"; "Namespaced"; "Module"; "Paths"; "Schematic" ],
            ["Lexer";"Syntaxerr"],[]);
          "standard_policies.ml", (["Fault"; "Standard_faults"; "Solver"],[],[]);
          "standard_policies.mli", (["Fault"],[],[]);
          "unit.mli", (["Deps";"Paths"; "M2l"; "Module"; "Namespaced";
                        "Fault"; "Read"],
                       ["Format";"Set"],[]);
          "unit.ml", (
            ["Approx_parser"; "Deps"; "M2l"; "Module"; "Namespaced";
             "Fault"; "Option"; "Paths"; "Pp"; "Read";
             "Standard_faults"],
            [ "List"; "Location"; "Set"],
            []);
          "support.ml", ([],["String"],[]);
          "support.mli", ([],[],[]);
          "with_deps.ml", (["Deps"],[],[]);
          "with_deps.mli", (["Deps"],[],[]);
          "transforms.ml", (
            ["Deps"; "Fault"; "Module"; "Name"; "Paths";"Standard_faults";
             "Summary"]
          ,[],[]);
          "stage.mli", (
            ["Deps"; "Fault"; "Loc"; "M2l";"Module"; "Namespaced"; "Paths";
             "Pp"; "Summary"; "Transforms"]
          ,["Format"],[]);
          "dep_zipper.ml", (
            ["Deps"; "Module"; "Stage"; "Transforms"; "Zipper"; "Zipper_fold"]
          ,[],[]);
          "zipper_fold.ml", (
            ["Deps"; "Loc"; "M2l"; "Module"; "Mresult"; "Option";
             "Paths"; "Stage";"Zipper_skeleton"; "Zipper_def";
             "Zipper_pp"]
          ,["List"],[]);
          "zipper_pp.ml", (
            ["Loc"; "M2l"; "Module"; "Name"; "Paths"; "Pp"; "Zipper_def"; "Zipper_skeleton"]
          ,["Format"],[]);
          "zipper_fold.mli", (["Stage";"Zipper_def"], [], []);
          "zipper.ml", ([],[],[]);
          "zipper_def.ml", (
            ["Deps"; "Loc"; "M2l";"Fault"; "Module"; "Name";"Paths";
             "Zipper_skeleton"]
          ,[],[]);
          "zipper_skeleton.ml", (
            ["Deps"; "Fault"; "M2l"; "Module"; "Name"; "Namespaced"; "Option"; "Paths"; "Pp";
             "Stage"; "Standard_faults"; "Summary"; "Transforms"]
          ,["Format"; "List"],[]);
          "zipper_skeleton.mli", (
            ["Deps"; "Fault"; "M2l"; "Module"; "Name"; "Paths"; "Pp"; "Stage";
             "Summary"; "Transforms"]
          ,[],[]);
        ])
      )
    )

let () =
  if result then
    Format.printf "Success.\n"
  else
    Format.printf "Failure.\n"
