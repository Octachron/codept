{ open Sig_parse }

let text = [^ '(' ')' ' ' '\n' '\t' ]
let space = [' ' '\t' '\n']

rule main = parse
  | "args" {ARGS}
  | "modules" {MODULES}
  | "module types" {MODULE_TYPES}
  | "(" {L}
  | ")" {R}
  | space+ { main lexbuf }
  | text+ as t {TEXT t}
  | eof {EOF}
