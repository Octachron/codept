
(* The type of tokens. *)

type token = 
  | R
  | L
  | EOF
  | ATOM of (string)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val sexp: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Sexp.any)

val many: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Sexp.many Sexp.t)

val keyed: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Sexp.one_and_many Sexp.t)

val atom: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Sexp.atomic Sexp.t)
