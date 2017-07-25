{ open Sparser
  let b = Buffer.create 17
}

let text = [^ '(' ')' '}' '{' ']' '[' ' ' '\n' '\t' ',' ':' '"' ]
let space = [' ' '\t' '\n']

rule main = parse
  | "(" {L}
  | ")" {R}
  | "[" {LS}
  | "]" {RS}
  | "{" {LC}
  | "}" {RC}
  | ":" {COLON}
  | "," {COMMA}
  | '"' { string lexbuf }
  | space+ { main lexbuf }
  | text+ as t { ATOM t }
  | eof {EOF}

and string = parse
  | "\\\"" as s { Buffer.add_string b s; string lexbuf }
  | '"' {let s = Buffer.contents b in Buffer.clear b; ATOM s }
  | _ as c {Buffer.add_char b c; string lexbuf}
