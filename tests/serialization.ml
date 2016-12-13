
let lex src = Lexing.from_string src

let (%) f g x = f (g x)

let parse_to_sexp = Sexp_parse.many Sexp_lex.main % lex

let round_trip_sexp (impl: _ Sexp.impl) =
  Option.fmap impl.embed % impl.parse

let show = Format.asprintf "%a" Sexp.pp

let full_round_trip sexp =
  Option.fmap show % round_trip_sexp sexp % parse_to_sexp

let test sexp s =
  match full_round_trip sexp s with
  | None -> Format.printf "Invalid sexp %s \n" s; false
  | Some s' ->
    let r = s = s' in
    if not r then
      ( Format.printf "Failure expected:⟨%s⟩, got ⟨%s⟩\n" s s' ; r )
    else
      r

let () =
  let r =
    List.for_all (test Sexp.(list @@ list int) )
      [ "((1 2 3) (4 5))"; "(1 2 3)"; "(1)" ]
   &&
    List.for_all (test (Sexp.list Module.sexp)) [
      "(Name)";
      "(First Second)";
      "((M (modules K L)))"
    ] in
  if r then
    Format.printf "Parsing round-trip success@."
  else
    Format.printf "Parsing round-trip failure@."
