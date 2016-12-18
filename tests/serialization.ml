
let lex src = Lexing.from_string src

let sg =
  "((Solver (modules (Failure modules Map Set) (Make args Envt Param)) (origin Unit (file lib solver.mli))) (Approx_parser origin Unit (file lib approx_parser.mli)) (Ast_converter origin Unit (file lib ast_converter.mli)) (Cmi origin Unit (file lib cmi.mli)) (Envts (module_types extended extended_with_deps) (modules Base (Interpreters modules (Sg args Param) (Tr args Param)) (Layered modules Envt) (Open_world args Envt) Tr (Tracing args Envt) Trl) (origin Unit (file lib envts.mli))) (Interpreter (module_types envt envt_with_deps param s with_deps) (modules (Make args Envt Param)) (origin Unit (file lib interpreter.mli))) (Unit (module_types (group modules Map) group_core) (modules (Groups modules (Filename modules Map) (Make (args Base) (modules Map)) (R modules Map) (Unit modules Map)) (+Alias Pkg Paths Pkg) (+Alias Pth Paths Simple) Set) (origin Unit (file lib unit.mli))) (Read origin Unit (file lib read.mli)) (M2l (modules Annot Block Build Normalize Sig_only) (origin Unit (file lib m2l.mli))) (Definition (modules Def) (origin Unit (file lib definition.mli))) (Fault (modules Level Log Polycy) (origin Unit (file lib fault.mli))) (Module (modules Arg Origin Partial Precision Sig) (origin Unit (file lib module.mli))) (Result origin Unit (file lib result.mli)) (Pp origin Unit (file lib pp.mli)) (Paths (modules E Expr (P modules Set) (Pkg modules Set) (S modules Map Set) (Simple modules Map Set)) (origin Unit (file lib paths.mli))) (Option origin Unit (file lib option.mli)) (Name (modules Map Set) (origin Unit (file lib name.mli))))"

let m2l =
"((Bind_sig (extended (Sig ((Open Interpreter) (SigInclude (Ident (A envt))) (Minor (access Module Name Paths)))))) (Bind_sig (extended_with_deps (Sig ((SigInclude (With ((Ident (A extended))))) (SigInclude (With ((Ident (S ((A Interpreter) with_deps)))))))))) (Bind (Base (Constraint (Abstract (Sig ((SigInclude (With ((Ident (A extended_with_deps))))))))))) (Bind (Open_world (Constraint (Abstract (Fun (((Envt (Ident (A extended_with_deps)))) (Sig ((Minor (access Envt Name Paths)) (SigInclude (With ((Ident (A extended_with_deps))))))))))))) (Bind (Layered (Constraint (Abstract (Sig ((Bind (Envt (Constraint (Abstract (With ((Ident (A extended)))))))) (Minor (access Base Envt Name Paths)) (SigInclude (With ((Ident (A extended))))))))))) (Bind (Tracing (Constraint (Abstract (Fun (((Envt (Ident (A extended)))) (Sig ((Minor (access Envt Paths)) (SigInclude (With ((Ident (A extended))))) (Minor (access Envt)))))))))) (Bind (Trl (Constraint (Abstract (Sig ((Minor (access Layered Paths Tracing)) (SigInclude (With ((Ident (A extended_with_deps))))) (Minor (access Layered)))))))) (Bind (Tr (Constraint (Abstract (Sig ((Minor (access Name Open_world Paths Trl)) (SigInclude (With ((Ident (A extended_with_deps))))) (Minor (access Name Paths Trl)))))))) (Bind (Interpreters (Constraint (Abstract (Sig ((Bind (Sg (Constraint (Abstract (Fun (((Param (Ident (S ((A Interpreter) param))))) (Sig ((Minor (access Base M2l)))))))))) (Bind (Tr (Constraint (Abstract (Fun (((Param (Ident (S ((A Interpreter) param))))) (Sig ((Minor (access Base M2l Tr)))))))))))))))))"


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
      "((M (args X) (modules K)))";
    ]
   && List.for_all (test keyed Module.Origin.sexp)
     ["(Unit (file dir a))"; "(Arg)"]
   && test many (Sexp.list Module.sexp) sg
   && test many M2l.sexp m2l
   && List.for_all (test many M2l.sexp) [
     "((Bind_sig (A (With ((Ident (A S)))))))"
   ]

  in
  if r then
    Format.printf "Parsing round-trip success@."
  else
    Format.printf "Parsing round-trip failure@."
