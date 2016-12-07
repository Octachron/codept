(** Ast_converter does the conversion between the OCaml parsetree AST and
    the M2l AST. *)

type t= { structure: Parsetree.structure -> M2l.t;
          signature: Parsetree.signature -> M2l.t
        }

val with_polycy: Messages.Polycy.t -> t
