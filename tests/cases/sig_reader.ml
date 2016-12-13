let src = "(Name(modules (Core) (Map) (Set)))"

let lex = Lexing.from_string src

let m = List.hd @@ Sig_parse.top_modules Sig_lex.main lex

let src' = Format.asprintf "%a" Module.persistent m

let () =
  if src = src' then
    Format.printf "Parsing round-trip success@."
  else
    Format.printf "Parsing round-trip failure@."
