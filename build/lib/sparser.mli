
(* The type of tokens. *)

type token = 
  | RS
  | RC
  | R
  | LS
  | LC
  | L
  | EOF
  | COMMA
  | COLON
  | ATOM of (string)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Schematic.Untyped.t)
