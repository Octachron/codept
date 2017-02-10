
let lex src = Lexing.from_string src

let sg =
  "((Solver (modules (Failure modules Map Set) (Make args Envt Param)) (origin Unit (file lib solver.mli))) (Approx_parser origin Unit (file lib approx_parser.mli)) (Ast_converter origin Unit (file lib ast_converter.mli)) (Cmi origin Unit (file lib cmi.mli)) (Envts (module_types extended extended_with_deps) (modules Base (Interpreters modules (Sg args Param) (Tr args Param)) (Layered modules Envt) (Open_world args Envt) Tr (Tracing args Envt) Trl) (origin Unit (file lib envts.mli))) (Interpreter (module_types envt envt_with_deps param s with_deps) (modules (Make args Envt Param)) (origin Unit (file lib interpreter.mli))) (Unit (module_types (group modules Map) group_core) (modules (Groups modules (Filename modules Map) (Make (args Base) (modules Map)) (R modules Map) (Unit modules Map)) (+Alias Pkg Paths Pkg) (+Alias Pth Paths Simple) Set) (origin Unit (file lib unit.mli))) (Read origin Unit (file lib read.mli)) (M2l (modules Annot Block Build Normalize Sig_only) (origin Unit (file lib m2l.mli))) (Definition (modules Def) (origin Unit (file lib definition.mli))) (Fault (modules Level Log Polycy) (origin Unit (file lib fault.mli))) (Module (modules Arg Origin Partial Precision Sig) (origin Unit (file lib module.mli))) (Result origin Unit (file lib result.mli)) (Pp origin Unit (file lib pp.mli)) (Paths (modules E Expr (P modules Set) (Pkg modules Set) (S modules Map Set) (Simple modules Map Set)) (origin Unit (file lib paths.mli))) (Option origin Unit (file lib option.mli)) (Name (modules Map Set) (origin Unit (file lib name.mli))))"

let m2l ="(((Bind_sig (extended (Sig (((Open Interpreter) (6 7 18)) ((SigInclude (Ident (A envt))) (7 2 14)) ((Minor (access (Deps ((14 24 35))) (Interpreter ((15 16 40))) (Module ((14 47 59))) (Name ((12 44 50))) (Paths ((12 21 35))))) (+Multiline 12 2 22 43)))))) (+Multiline 4 0 27 3)) ((Bind_sig (extended_with_deps (Sig (((SigInclude (With ((Ident (A extended))))) (32 2 33)) ((SigInclude (With ((Ident (S ((A Interpreter) with_deps)))))) (33 2 48)))))) (+Multiline 29 0 34 3)) ((Bind (Base (Constraint (Abstract (Sig (((Minor (access (Module ((39 18 35))))) (39 2 64)) ((SigInclude (With ((Ident (A extended_with_deps))))) (40 2 45)) ((Minor (access (Module ((42 13 30))))) (42 2 35)))))))) (+Multiline 37 0 43 3)) ((Bind (Open_world (Constraint (Abstract (Fun (((Envt (Ident (A extended_with_deps)))) (Sig (((Minor (access (Deps ((51 24 30))) (Envt ((49 21 27))) (Name ((50 22 30))))) (+Multiline 49 4 51 37)) ((SigInclude (With ((Ident (A extended_with_deps))))) (53 4 47)) ((Minor (access (Envt ((54 15 21))) (Name ((54 25 33))))) (54 4 38)))))))))) (+Multiline 46 0 55 3)) ((Bind (Layered (Constraint (Abstract (Sig (((Bind (Envt (Constraint (Abstract (With ((Ident (A extended)))))))) (61 2 56)) ((Minor (access (Base ((67 21 27))) (Envt ((64 22 28))) (Name ((65 22 30))) (Paths ((63 12 26))))) (+Multiline 62 2 69 53)) ((SigInclude (With ((Ident (A extended))))) (70 2 35)))))))) (+Multiline 59 0 72 3)) ((Bind (Tracing (Constraint (Abstract (Fun (((Envt (Ident (A extended)))) (Sig (((Minor (access (Deps ((78 38 44))) (Envt ((78 23 29))))) (78 6 51)) ((SigInclude (With ((Ident (A extended_with_deps))))) (79 6 49)) ((Minor (access (Envt ((80 18 24))))) (80 6 29)))))))))) (+Multiline 75 0 81 7)) ((Bind (Trl (Constraint (Abstract (Of (Apply ((Ident Tracing) (Ident Layered)))))))) (83 0 43)) ((Bind (Tr (Constraint (Abstract (Of (Apply ((Ident Open_world) (Ident Trl)))))))) (84 0 41)))"

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
      ( Format.printf "Failure expected:\n%s, got\n%s\n" s s' ; r )
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
