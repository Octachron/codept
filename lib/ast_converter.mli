(** Ast_converter does the conversion between the OCaml parsetree AST and
    the M2l AST. *)

val structure: Parsetree.structure -> M2l.t

val signature: Parsetree.signature -> M2l.t
