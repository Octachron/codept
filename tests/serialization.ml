
let lex src = Lexing.from_string src

let sg =
  "((Solver (modules (Failure modules Map Set) (Make args Envt Param)) (origin Unit (file lib solver.mli))) (Approx_parser origin Unit (file lib approx_parser.mli)) (Ast_converter origin Unit (file lib ast_converter.mli)) (Cmi origin Unit (file lib cmi.mli)) (Envts (module_types extended extended_with_deps) (modules Base (Interpreters modules (Sg args Param) (Tr args Param)) (Layered modules Envt) (Open_world args Envt) Tr (Tracing args Envt) Trl) (origin Unit (file lib envts.mli))) (Interpreter (module_types envt envt_with_deps param s with_deps) (modules (Make args Envt Param)) (origin Unit (file lib interpreter.mli))) (Unit (module_types (group modules Map) group_core) (modules (Groups modules (Filename modules Map) (Make (args Base) (modules Map)) (R modules Map) (Unit modules Map)) (+Alias Pkg Paths Pkg) (+Alias Pth Paths Simple) Set) (origin Unit (file lib unit.mli))) (Read origin Unit (file lib read.mli)) (M2l (modules Annot Block Build Normalize Sig_only) (origin Unit (file lib m2l.mli))) (Definition (modules Def) (origin Unit (file lib definition.mli))) (Fault (modules Level Log Polycy) (origin Unit (file lib fault.mli))) (Module (modules Arg Origin Partial Precision Sig) (origin Unit (file lib module.mli))) (Result origin Unit (file lib result.mli)) (Pp origin Unit (file lib pp.mli)) (Paths (modules E Expr (P modules Set) (Pkg modules Set) (S modules Map Set) (Simple modules Map Set)) (origin Unit (file lib paths.mli))) (Option origin Unit (file lib option.mli)) (Name (modules Map Set) (origin Unit (file lib name.mli))))"

let m2l =
  "(((Bind_sig (extended (Sig (((Open Interpreter) (6 7 18)) ((SigInclude (Ident (A envt))) (7 2 14)) ((Minor (access Module Name Paths)) (+Multiline 12 2 21 43)))))) (+Multiline 4 0 26 3)) ((Bind_sig (extended_with_deps (Sig (((SigInclude (With ((Ident (A extended))))) (31 2 33)) ((SigInclude (With ((Ident (S ((A Interpreter) with_deps)))))) (32 2 48)))))) (+Multiline 28 0 33 3)) ((Bind (Base (Constraint (Abstract (Sig (((Minor (access Module)) (38 2 63)) ((SigInclude (With ((Ident (A extended_with_deps))))) (39 2 45)) ((Minor (access Module)) (41 2 34)))))))) (+Multiline 36 0 42 3)) ((Bind (Open_world (Constraint (Abstract (Fun (((Envt (Ident (A extended_with_deps)))) (Sig (((Minor (access Envt Name Paths)) (+Multiline 48 4 50 44)) ((SigInclude (With ((Ident (A extended_with_deps))))) (52 4 47)))))))))) (+Multiline 45 0 53 3)) ((Bind (Layered (Constraint (Abstract (Sig (((Bind (Envt (Constraint (Abstract (With ((Ident (A extended)))))))) (59 2 56)) ((Minor (access Base Envt Name Paths)) (+Multiline 60 2 67 53)) ((SigInclude (With ((Ident (A extended))))) (68 2 35)))))))) (+Multiline 57 0 70 3)) ((Bind (Tracing (Constraint (Abstract (Fun (((Envt (Ident (A extended)))) (Sig (((Minor (access Envt Paths)) (76 6 58)) ((SigInclude (With ((Ident (A extended))))) (77 6 39)) ((Minor (access Envt)) (78 6 29)))))))))) (+Multiline 73 0 79 7)) ((Bind (Trl (Constraint (Abstract (Sig (((Minor (access Layered Paths Tracing)) (+Multiline 83 2 84 50)) ((SigInclude (With ((Ident (A extended_with_deps))))) (85 2 44)) ((Minor (access Layered)) (86 2 28)))))))) (+Multiline 81 0 87 3)) ((Bind (Tr (Constraint (Abstract (Sig (((Minor (access Name Open_world Paths Trl)) (+Multiline 92 2 95 36)) ((SigInclude (With ((Ident (A extended_with_deps))))) (97 2 45)) ((Minor (access Name Paths Trl)) (98 2 48)))))))) (+Multiline 90 0 99 3)) ((Bind (Interpreters (Constraint (Abstract (Sig (((Bind (Sg (Constraint (Abstract (Fun (((Param (Ident (S ((A Interpreter) param))))) (Sig (((Minor (access Base M2l Module)) (+Multiline 105 10 106 49)))))))))) (+Multiline 103 4 106 53)) ((Bind (Tr (Constraint (Abstract (Fun (((Param (Ident (S ((A Interpreter) param))))) (Sig (((Minor (access M2l Module Tr)) (109 12 78)))))))))) (+Multiline 107 4 109 82)))))))) (+Multiline 101 0 110 5)))"


let m2l2 ="(((Minor (values (((Extension_node (expr Module)) (3 3 12))) (((Extension_node (pat Val)) (2 6 17))))) (+Multiline 2 0 3 12)))"


let orec = "(((Bind (M (Constraint (Abstract (Fun (() Sig)))))) (1 0 19)))"
let orec2 = "(((Bind (A Constraint)) (1 0 17)) ((Bind (M (Constraint (Abstract (Fun (() (Sig (((SigInclude (Of (Ident A))) (2 16 42)))))))))) (2 0 46)))"

let (%) f g x = f (g x)

let many = Sexp_parse.many
let keyed = Sexp_parse.keyed

let parse_to_sexp kind = kind Sexp_lex.main % lex

let round_trip_sexp (impl: _ Sexp.impl) =
  Option.fmap impl.embed % impl.parse

let show x = Format.asprintf "%a" Sexp.pp x

let full_round_trip kind sexp =
  Option.fmap show % round_trip_sexp sexp % parse_to_sexp kind

let test kind sexp s =
  match full_round_trip kind sexp s with
  | None -> Format.printf "Invalid sexp %s \n" s; false
  | Some s' ->
    let r = s = s' in
    if not r then
      ( Format.printf "Failure expected:⟨%s⟩, got ⟨%s⟩\n" s s' ; r )
    else
      r

let () =
  let r =
    List.for_all (test many Sexp.(list @@ list int) )
      [ "((1 2 3) (4 5))"; "(1 2 3)"; "(1)" ]
   &&
    List.for_all (test many (Sexp.list Module.sexp)) [
      "(Name)";
      "(First Second)";
      "((M modules K L))";
      "((A (modules (F args ())) (origin Unit (file a.ml))))";
      "((M (args X) (modules K)))";
    ]
   && List.for_all (test keyed Module.Origin.sexp)
     ["(Unit (file dir a))"; "(Arg)"]
   && test many (Sexp.list Module.sexp) sg
   && List.for_all (test many M2l.sexp) [m2l; m2l2; orec; orec2]
   && List.for_all (test many M2l.sexp) [
     "(((Bind_sig (A (With ((Ident (A S)))))) (1 0 35)))"
   ]

  in
  if r then
    Format.printf "Parsing round-trip success@."
  else
    Format.printf "Parsing round-trip failure@."
