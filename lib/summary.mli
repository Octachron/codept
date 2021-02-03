(** Short summary of visible and defined modules at an AST node *)

type view

type t = {
  defined: view; (** modules and module types defined in scope *)
  visible: view; (** in scope but not defined *)
}
type summary = t


module View: sig

  val empty: view
  val is_empty: view -> bool

  val see : Module.signature -> summary
  val define : Module.signature -> summary
  val merge : view -> view -> view

  val pp: view Pp.t
  val sch: view Schematic.t

end


val pp : Format.formatter -> summary -> unit
(** pretty printer *)

val sch: t Schematic.t

val empty : summary
(** Empty summary *)

val defined : summary -> Module.signature
(** Modules defined in the current scope *)

val extend: Module.signature -> summary -> Module.signature

val only_visible : summary -> view
(** List modules that are visibles and not defined *)

val clear_visible : summary -> summary
(** forget visible but not defined modules *)

(** Basic creation function *)
val define: ?level:Module.level -> Module.named list -> summary

(** {2 Merging functions } *)

val merge: summary -> summary -> summary
val ( +| ) : summary -> summary -> summary


(** {2 Basic summary extension} *)

val bind: ?level:Module.level -> Module.named -> summary -> summary
(** [bind m def] binds the module [m] in [def] *)

val see : Module.named -> summary -> summary
(** [see m def] makes the module [m] visible in [def] *)

val binds : (Module.level * Module.named) list -> summary
(** [binds [level,m;…] def] binds the modules [m] at [level]
    in [def] *)

val of_partial :  Module.Partial.t -> (summary,summary) result
(** Create a summary from a partial module *)
