let src = "(Name(modules (Core) (Map) (Set)))"

let lex = Lexing.from_string src

let m = Sig_parse.top_module Sig_lex.main lex

let src' = Format.asprintf "%a" Module.persistent m

let () =
  if src = src' then
    Format.printf "Parsing round-trip success@."
  else
    Format.printf "Parsing round-trip failure@."
