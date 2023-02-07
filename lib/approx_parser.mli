(** Approximate parser for non-syntactically valid files.

   Heuristic: try to detect
   - open Module_path
   - include Module_path
   - Module_path . not module identifier
   - module M
   and match them to the corresponding toplevel structure item.

   As a consequence, the set of bound modules at any point in the approximated
   m2l ast should be a superset of the set of bound module in the wished AST.
   Notwithstanding inclusion of UIDENT signature, the dependencies inferred
   for the corresponding AST shall yield a lower bound for the wished dependencies.

   We can also transform the corresponding approximated AST to eliminate all
   binding and convert opening and including modules to module access. Infering
   dependencies from this AST will gives us an upper bound of wished dependencies.
 **)


(** parse the lower_bound (* for dependency *) ast *)
val lower_bound: string -> M2l.t

(** Conver a lower dependencies bound ast to an upper dependencencies bound
    by removing binding, and simplyfing open and includes. *)
val to_upper_bound: M2l.t -> M2l.t

(** [file filename] yields [module_name × lower bound × upper bound] *)
val file: string -> Modname.t * M2l.t * M2l.t
