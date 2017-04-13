(** Short summary of visible and defined modules at an AST node *)

module Namespace: sig
  type t = Paths.S.t
  type tree =
    | Node of tree Name.map
    | Leaf of Module.definition
end

(** Invariant: defined ⊂ visible *)
type t = {
  defined: Module.signature;
  visible:Module.signature;
  namespaces:Namespace.tree Name.map
}
type summary = t

val pp : Format.formatter -> summary -> unit
(** pretty printer *)

val sexp: (t, Sexp.many) Sexp.impl

val empty : summary
(** Empty summary *)

val defined: summary -> Module.signature

val only_visible : summary -> Module.signature
(** List modules that are visibles and not defined *)

val sg_bind : Module.signature -> summary
(** define the given signature *)

val sg_see : Module.signature -> summary
(** make visible the given signature *)

val in_namespace: Namespace.t -> summary -> summary 

val clear_visible : summary -> summary
(** forget visible but not defined modules *)

(** Basic creation functions *)
module Def :
  sig

    val md : Module.t -> summary
    (** [md m] Define the module [m] *)

    val mods : Module.t list -> summary
    (** [md [m;…]] Define the modules [m;…] *)

    val sg : Module.t -> summary
    (** [sg s] Define the module type [s] *)


    val sgs : Module.t list -> summary
    (** [sg s_list] Define the module types [s_list] *)

    val gen : Module.level -> Module.t -> summary
    (** [gen level m] define the module [m] at [level] *)

    val ( +@ ) : Module.signature -> Module.signature -> Module.signature
    (** merge signatures *)

    val merge: summary -> summary -> summary
    val ( +| ) : summary -> summary -> summary
    (** merge summaries *)

  end

(** {2 Basic summary extension} *)

val bind : ?namespace:Namespace.t -> Module.t -> summary -> summary
(** [bind m def] binds the module m in [def] *)

val see : ?namespace:Namespace.t -> Module.t -> summary -> summary
(** [see m def] makes the module [m] visible in [def] *)

val bind_sg : ?namespace:Namespace.t -> Module.t -> summary -> summary
(** [bind_sg s def] makes the module type [m] visible in [def] *)

val bind_gen :
  ?namespace:Namespace.t -> Module.level -> Module.t -> summary
  -> summary
(** [bind_gen level m def] binds the module [m] at [level] in [def] *)

val binds : (Module.level * Namespace.t * Module.t) list -> summary
(** [binds [level,m;…] def] binds the modules [m] at [level] 
    in [def] *)

val of_partial : ?namespace:Namespace.t -> Module.Partial.t
  -> (summary,summary) result
(** Create a summary from a partial module *)
