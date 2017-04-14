(** Short summary of visible and defined modules at an AST node *)

module Namespace: sig
  type t = Paths.S.t
  type tree =
    | Node of tree Name.map
    | Leaf of Module.signature
end

type view = { local: Module.signature;
              namespace: Namespace.tree Name.map }
type t = {
  defined: view; (** modules and module types defined in scope *)
  visible: view; (** in scope but not defined *)
}
type summary = t


module View: sig

  val empty: view
  val is_empty: view -> bool
  val make: ?namespace:Namespace.t -> Module.signature -> view
  
  val merge : view -> view -> view
  
  val see : view -> summary
  val define : view -> summary

end
  

val pp : Format.formatter -> summary -> unit
(** pretty printer *)

val sexp: (t, Sexp.many) Sexp.impl

val empty : summary
(** Empty summary *)

val defined : summary -> view
val local: view -> Module.signature

val only_visible : summary -> view
(** List modules that are visibles and not defined *)

val clear_visible : summary -> summary
(** forget visible but not defined modules *)

(** Basic creation function *)
  val define: ?level:Module.level -> Module.t list -> summary

(** {2 Merging functions } *)
  
val merge: summary -> summary -> summary
val ( +| ) : summary -> summary -> summary


(** {2 Basic summary extension} *)

val bind : ?namespace:Namespace.t -> ?level:Module.level
  -> Module.t -> summary -> summary
(** [bind m def] binds the module m in [def] *)

val see : ?namespace:Namespace.t -> Module.t -> summary -> summary
(** [see m def] makes the module [m] visible in [def] *)

val binds : (Module.level * Namespace.t * Module.t) list -> summary
(** [binds [level,m;…] def] binds the modules [m] at [level] 
    in [def] *)

val of_partial : ?namespace:Namespace.t -> Module.Partial.t
  -> (summary,summary) result
(** Create a summary from a partial module *)
