{ open Sexp_parse }

let text = [^ '(' ')' ' ' '\n' '\t' ]
let space = [' ' '\t' '\n']

rule main = parse
  | "(" {L}
  | ")" {R}
  | space+ { main lexbuf }
  | text+ as t {TEXT t}
  | eof {EOF}
