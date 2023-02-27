
let error = ref false
let ok ppf = Format.fprintf ppf "\x1b[32mok\x1b[0m"
let failure ppf = Format.fprintf ppf "\x1b[31mfailure\x1b[0m"
let fail fmt =
  Format.kfprintf (fun ppf -> error := true; Format.fprintf ppf "[%t]@." failure; error := true)
    Format.std_formatter fmt
let log fmt =
  Format.ikfprintf (fun ppf -> Format.ifprintf ppf "[%t]@." ok) Format.std_formatter fmt



let local = Pkg.local

let root = if Array.length Sys.argv > 0 then Some (Sys.argv.(1)) else None

let (%) f g x = f (g x)
let (~:) = List.map (fun str ->
  match List.rev (Support.split_on_char '.' str) with
  | [] -> assert false
  | name :: nms -> Namespaced.make ~nms:(List.rev nms) name)

let classify filename =  match Support.extension filename with
  | "ml" -> { Read.format= Src; kind  = M2l.Structure }
  | "mli" -> { Read.format = Src; kind = M2l.Signature }
  | ext -> raise (Invalid_argument ("unknown extension: "^ext))

module Version = struct
  let major, minor = Scanf.sscanf Sys.ocaml_version "%d.%d" (fun x y -> x, y)
  type t = {major:int; minor:int}
  let v = { major; minor }

  let v_4_04 = { minor = 4;  major = 4 }
  let v_4_06 = { minor = 6;  major = 4 }
  let v_4_07 = { minor = 7;  major = 4 }
  let v_4_08 = { minor = 8;  major = 4 }
  let v_4_13 = { minor = 13; major = 4 }

end


let policy =
  Standard_policies.quiet

let read policy (info,f,path) =
  Unit.read_file policy info f path

let read_simple policy (info,f) =
  read policy (info, f, Namespaced.filepath_of_filename f)

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
      type t = Namespaced.t list
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
        | Ok (e,_) ->
          e, cycles
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
    if r then
      log "%a" Pkg.pp name
    else
      fail "Simple test failure: %a(%s).@;<0 2>Expected:[%a], got:@[[%a]@]"
        Pkg.pp name (Sys.getcwd ())
        Pp.(list Pkg.pp) (List.sort compare list)
        Pp.(list Pkg.pp) (normalize set);
    r

  let (%) f g x = f (g x)

  let normalize_2 set =
    let l = Deps.pkgs set in
    let is_inner =
      function { Pkg.source = Local; _ } -> true | _ -> false in
    let is_lib =
      function { Pkg.source = (Pkg _ | Special _ ) ; _ } -> true
             | _ -> false in
    let inner, rest = List.partition is_inner l in
    let lib, unkn = List.partition is_lib rest in
    let norm = List.sort compare % List.map Pkg.module_name in
    norm inner, norm lib, norm unkn


  let precise_deps_test name (inner,lib,unkw) set =
    let sort = List.sort compare in
    let inner, lib, unkw = sort inner, sort lib, sort unkw in
    let inner', lib', unkw' = normalize_2 set in
    let test subtitle x y =
      let r = x = y  in
      if r then
        log "%a/%s" Pkg.pp name subtitle
      else
        fail "Failure %a %s: expected:[%a], got:@[[%a]@]\n"
          Pkg.pp name subtitle
          Pp.(list estring) x
          Pp.(list estring) y;
      r
    in
    test "local" inner (List.map Modname.to_string inner')
    && test "lib" lib (List.map Modname.to_string lib')
    && test "unknown" unkw (List.map Modname.to_string unkw')

  let add_info {Unit.ml; mli} ((f,p), info) =
    let k = classify f in
    let x = (k,f,p, info) in
    match k.kind with
    | M2l.Structure -> { Unit.ml = x :: ml; mli }
    | M2l.Signature -> { Unit.mli = x :: mli; ml }

  let add_file {Unit.ml; mli} file =
    let k = classify file in
    let x = k, file, Namespaced.module_path_of_filename file in
    match k.kind with
    | M2l.Structure -> { Unit.ml = x :: ml; mli }
    | M2l.Signature -> { Unit.mli = x :: mli; ml }

  let gen_deps_test libs inner_test roots l =
    let {Unit.ml;mli} =
      List.fold_left add_info {Unit.ml=[]; mli=[]} l in
    let module M = Pkg.Map in
    let build exp = List.fold_left (fun m (_,f,_n,l) ->
        M.add (Pkg.local f) l m)
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

  let compiler_libs =
    let cmd = "ocamlc -where " in
    let cin = Unix.open_process_in cmd in
    try
      [Filename.concat (input_line cin) "compiler-libs" ]
    with
      End_of_file -> []

  let cycle_test name expected l =
    let files = List.fold_left add_file {Unit.ml=[]; mli=[]} l in
    let cycles = analyze_cycle files in
      let expected = List.map (List.sort compare) expected in
      let cycles = List.map (List.sort compare) (CSet.elements cycles) in
      let r = List.map2 (List.map2 Namespaced.compare) cycles expected in
      let r = List.for_all (List.for_all ((=) 0)) r in
      if r then
        log "cycle:%s" name
      else
        ( fail "Cycle %s failure:@;<0 2>expected %a, got %a"
            name
            Pp.(list @@ list Namespaced.pp) expected
            Pp.(list @@ list Namespaced.pp) cycles;
         );
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

let l = List.map Pkg.local
let u = List.map (fun x -> Pkg.{(local x) with source=Unknown})
let n = List.map Namespaced.make

let std =
  let localize x = { Pkg.source = Special "stdlib"; file = Namespaced.make ~nms:["Stdlib"] x } in
  List.map localize

let d x =
  x,
  let nms = List.map String.capitalize_ascii @@ Support.split_on_char '/' x in
  Namespaced.of_path nms

let (/) p x =
  x, Namespaced.module_path_of_filename ~nms:[p] x

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
    ["with_more.ml", u["Ext";"Ext3"]];
    ["alias_in_with.ml", u["Ext";"Ext2"; "Ext3"]]
  ]

  && ( Version.( v < v_4_04) ||
       Std.deps_test_single [d"pattern_open.ml", u["A'";"E1"; "E2"; "E3";"E4"]] )

  && ( Version.( v < v_4_08) ||
       Std.deps_test_single [d"option_monad.ml", u["Ext";"Ext3";"Ext4"]]
     )
  && ( Version.(v < v_4_13) ||
       Std.deps_test_single [d"with_module_type.ml", []]
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
  (* TODO(dinosaure): difficult to solve this error. *)
  (* && (chdir "../aliases_and_map";
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
     ) *)

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
             d "a.ml", l["b.ml"];
             "N" / "b.ml", [];
             d "c.ml", l ["b.ml"]
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
      Std.cycle_test "self" [n["Self_cycle"]] ["self_cycle.ml"]
    )
    &&
    (
      chdir "../complex/ω-cycle";
      Std.cycle_test "ω-cycle"
        [n["C1";"C2";"C3";"C4";"C5"]]
        [
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
      Std.cycle_test "2+2" [n["A";"B"]; n["C";"D"]] [ "a.ml" ; "b.ml"; "c.ml"; "d.ml"]
    )
    && (
      chdir "../8-cycles";
      Std.cycle_test "8-cycles" [ n["A";"B";"C";"D"]; n["A"; "H"; "G"; "F"; "E"]]
        [ "a.ml" ; "b.ml"; "c.ml"; "d.ml"; "e.ml"; "f.ml"; "g.ml"; "h.ml"]
    )
    && (
      chdir "../aliased_cycle";
      Std.cycle_test "aliased" [ n["A"; "B"; "C"] ]
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
    (
      chdir "../aliases_and_with";
      both ["A1";"Main"] @@ dl [
        "a1.ml", [];
        "main.ml", l["a1.ml"]  ;
      ]
    )
    &&
    ( chdir "../../../lib";
      Std.gen_deps_test Std.compiler_libs Std.precise_deps_test
        (Some ~:["Solver"; "Standard_policies"])
        (dl[
          "ast_converter.mli", ( ["M2l"], ["Parsetree"], [] );
          "approx_parser.mli", (["M2l";"Unitname"], [],[]);
          "approx_parser.ml", (["Deps"; "Loc"; "Read";"M2l";"Paths"],
                               ["Lexer"; "Parser";"Lexing";"List"],[]);
          "debug.ml", (["Pp"], ["Format";"Sys";"Printexc"], []);
          "cmi.mli", (["M2l"], [], []);
          "modname.ml", (["Support"; "Pp"], ["Format"; "Map"; "String"], []);
          "modname.mli", (["Pp"], ["Map"], []);
          "unitname.ml", (["Modname"; "Pp"; "Support"], ["Bytes"; "Filename"; "Format"; "Map"; "Set"; "String"], []);
          "unitname.mli", (["Modname"; "Pp"], ["Map"; "Set"], []);

          "deps.ml", (["Namespaced"; "Option"; "Pkg"; "Pp"; "Schematic"],["List"],[]);
          "deps.mli", (["Namespaced"; "Pkg"; "Pp"; "Schematic"],["Format"],[]);
          "summary.mli", (
            ["Module";"Schematic"; "Pp"],
            ["Format"],
            [] );
          "summary.ml", (
            ["Module"; "Pp"; "Schematic"],
            ["List"], []);
          "envt.mli", (
            ["Module";"Name"; "Namespaced"; "Stage"; "Paths";
             "Transforms"; "Uloc"],
            [], []);
          "envt.ml", (
            ["Cmi"; "Debug"; "Deps"; "Summary"; "Transforms";
             "Fault"; "Dep_zipper"; "Modname"; "Unitname"; "Module"; "Name"; "Namespaced"; "Paths";
             "Standard_faults";"Standard_policies";"Option"; "Pkg"; "Pp"; "Uloc"],
            ["Array"; "Filename";"List";"Sys"],
            []);
          "m2l.mli", (["Deps";"Loc"; "Module";"Name";"Paths"; "Pp"; "Schematic" ],
                      ["Format"],[]);
          "m2l.ml", (["Loc"; "Deps"; "Module"; "Name";
                      "Option";"Paths"; "Pp"; "Schematic"; "Schematic_indices" ],
                     ["List"],[]);
          "fault.ml", (["Format_tags"; "Option"; "Pp"],
                          ["Array"; "Format"; "Map"; "String"],[]);
          "fault.mli", ([], ["Format"],[]);
          "format_tags.mli", ([],["Format"],[]);
          "format_compat.mli", ([],["Format"],[]);
          "id.mli", ( ["Pkg";"Pp"; "Schematic"],
                          [], [] );
          "id.ml", ( ["Pkg";"Pp"; "Schematic"],
                          [], [] );
          "module.mli", ( ["Id"; "Paths"; "Pkg"; "Name";"Namespaced"; "Schematic"; "Uloc"],
                          ["Format"], [] );
          "module.ml", ( ["Id"; "Loc";"Paths"; "Pkg"; "Modname"; "Unitname"; "Name"; "Namespaced"; "Option"; "Pp"
                         ; "Schematic"; "Schematic_indices"; "Uloc" ],
                         ["List"; "Format"; "Map"], [] );
          "name.mli", ( [], ["Format";"Set";"Map"], [] );
          "name.ml", ( ["Pp"], ["Set";"Map";"List"], [] );
          "namespaced.mli", ( ["Modname";"Unitname";"Name";"Paths"; "Pp"; "Schematic"],
                              ["Format"; "Set";"Map"], [] );
          "namespaced.ml", ( ["Modname"; "Unitname"; "Paths"; "Pp"; "Schematic"; "Support"],
                             ["Filename"; "Format";"List";"Set";"Map"; "String"], [] );
          "loc.mli", ( ["Schematic"], ["Format"], []);
          "loc.ml", ( ["Pp";"Schematic"], ["List"], []);
          "uloc.mli", ( ["Loc"; "Pkg"; "Pp"], [], []);
          "uloc.ml", ( ["Format_tags"; "Loc"; "Namespaced"; "Pkg"; "Pp"], [], []);
          "option.mli", ([],["Lazy"],[]);
          "option.ml", ([],["List";"Lazy"],[]);
          "pparse_compat.mli", ([], ["Parsetree"], []);
          "paths.mli", (["Name"; "Pp"; "Schematic"], ["Map";"Set";"Format"],[]);
          "paths.ml", (["Name"; "Pp"; "Schematic"; "Schematic_indices"; "Support" ],
                       ["Filename";"List";"Map";"Set";"Format";
                        "String"],[]);
          "pp.mli", ([], ["Format"],[]);
          "pp.ml", ([], ["Format"],[]);
          "read.mli", (["M2l"; "Unitname"; "Schematic"],["Lexer";"Syntaxerr"],[]);
          "read.ml", (["Ast_converter"; "Cmi"; "M2l"; "Pparse_compat";"Schema"
                      ; "Unitname"; "Schematic"; ],
                      ["Lexer"; "Lexing"; "List"; "Location";
                       "Parse"; "Parsetree"; "Parsing"; "Pparse";
                       "Syntaxerr"],
                      ["Sparser";"Slex"]);
          "mresult.mli", ([],[],[]);
          "mresult.ml", ([],["List"],[]);
          "schema.ml", (["M2l"; "Module"; "Namespaced"; "Option"; "Schematic"],
                        [],  []);
          "schema.mli", (["M2l"; "Module"; "Namespaced"; "Schematic"], [], []);
          "schematic.mli", ([],["Format"], []);
          "schematic_indices.ml", (["Schematic"],[],[]);

          "schematic.ml", (["Format_compat"; "Name"; "Mresult"; "Pp";
                            "Support";"Option"],
                           ["Format"; "Hashtbl"; "List"; "Map"; "String"],
                           [] );
          "solver.mli", (["Fault"; "Loc"; "Unit";
                          "Namespaced"; "Read";
                          "Summary"; "Stage"; "Paths"; "Pkg"],
                         ["Format"],[]);
          "solver.ml", (
            ["Approx_parser"; "Debug"; "Deps"; "Summary"; "Stage"; "Loc";
             "M2l"; "Modname"; "Unitname"; "Module"; "Mresult"; "Namespaced";
             "Option"; "Pp"; "Paths"; "Pkg"; "Read"; "Unit"; "Fault";
             "Standard_faults"; "Uloc"],
            ["List";"Filename";"Map";"Format";"String"],[]);
          "standard_faults.ml", (
            ["Fault"; "Format_tags"; "Module"; "Namespaced"; "Paths"; "Pp"
            ; "Pkg"; "Loc"; "Schematic"; "Uloc" ],
            ["Format"; "Location"; "Syntaxerr"],[]);
          "standard_faults.mli", (
            ["Fault"; "Name"; "Namespaced"; "Module"; "Paths"; "Pkg"; "Schematic"; "Uloc" ],
            ["Lexer";"Syntaxerr"],[]);
          "standard_policies.ml", (["Fault"; "Standard_faults"; "Solver"],[],[]);
          "standard_policies.mli", (["Fault"],[],[]);
          "unit.mli", (["Deps";"Pkg"; "M2l"; "Module"; "Namespaced";
                        "Fault"; "Read"],
                       ["Format";"Set"],[]);
          "unit.ml", (
            ["Approx_parser"; "Deps"; "M2l"; "Module"; "Namespaced";
             "Fault"; "Option"; "Pkg"; "Pp"; "Read";
             "Standard_faults"],
            [ "List"; "Location"; "Set"],
            []);
          "support.ml", ([],["String"; "List"; "Sys"],[]);
          "support.mli", ([],[],[]);
          "with_deps.ml", (["Deps"],[],[]);
          "with_deps.mli", (["Deps"],[],[]);
          "transforms.ml", (
            ["Debug"; "Deps"; "Fault"; "Module"; "Name"; "Paths"; "Pp"; "Standard_faults";
             "Summary"]
          ,[],[]);
          "stage.mli", (
            ["Deps"; "Fault"; "Loc"; "M2l";"Module"; "Name"; "Namespaced"; "Paths"; "Pkg";
             "Pp"; "Summary"; "Transforms"; "Uloc"]
          ,["Format"],[]);
          "dep_zipper.ml", (
            ["Deps"; "Module"; "Stage"; "Transforms"; "Zipper"; "Zipper_fold"]
          ,[],[]);
          "zipper_fold.ml", (
            ["Debug"; "Deps"; "Id"; "Loc"; "M2l"; "Module"; "Mresult"; "Option";
             "Paths"; "Pkg"; "Stage"; "Summary"; "Zipper_skeleton"; "Zipper_def";
             "Zipper_pp"; "Uloc"]
          ,["List"],[]);
          "zipper_pp.ml", (
            ["Loc"; "M2l"; "Module"; "Name"; "Option"; "Paths"; "Pp"; "Zipper_def"; "Zipper_skeleton"]
          ,["Format"],[]);
          "zipper_fold.mli", (["Stage";"Zipper_def"], [], []);
          "zipper.ml", ([],[],[]);
          "zipper_def.ml", (
            ["Deps"; "Loc"; "M2l"; "Uloc"; "Module"; "Name";"Paths";
             "Zipper_skeleton"]
          ,[],[]);
          "zipper_skeleton.ml", (
            ["Debug"; "Deps"; "Fault"; "Id"; "M2l"; "Module"; "Name"; "Namespaced"; "Option"; "Paths"; "Pp";
             "Stage"; "Standard_faults"; "Summary"; "Transforms"; "Uloc"]
          ,["Format"; "List"],[]);
          "zipper_skeleton.mli", (
            ["Deps"; "Id"; "M2l"; "Module"; "Name"; "Paths"; "Pp"; "Stage";
             "Summary"; "Transforms"; "Uloc"]
          ,[],[]);
          "pkg.ml", (["Name"; "Unitname"; "Namespaced"; "Pp"; "Schematic"],["Filename"; "Map"; "Set"; "String"],[]);
          "pkg.mli", (["Modname"; "Name"; "Namespaced"; "Paths"; "Schematic"],["Format"; "Map"; "Set"],[]);
        ])
      )
    )

let () =
  if result && not !error then
    (Format.printf "Success.\n"; exit 0)
  else
    (Format.printf "Failure.\n"; exit 1)
