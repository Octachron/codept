(** Short summary of visible and defined modules at an AST node *)

type t = { defined : Module.signature; visible: Module.signature; }
type summary = t
(** Invariant: defined ⊂ visible *)

val pp : Format.formatter -> summary -> unit
(** pretty printer *)

val empty : summary
(** Empty summary *)

val only_visible : summary -> Module.signature
(** List modules that are visibles and not defined *)

val sg_bind : Module.signature -> summary
(** define the given signature *)

val sg_see : Module.signature -> summary
(** make visible the given signature *)

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

    val ( +| ) : summary -> summary -> summary
    (** merge summaries *)

  end

(** {2 Basic summary extension} *)

val bind : Module.t -> summary -> summary
(** [bind m def] binds the module m in [def] *)

val see : Module.t -> summary -> summary
(** [bind m def] makes the module [m] visible in [def] *)

val bind_sg : Module.t -> summary -> summary
(** [bind s def] makes the module type [m] visible in [def] *)

val bind_gen : Module.level -> Module.t -> summary -> summary
(** [bind level m def] binds the module [m] at [level] in [def] *)

val binds : (Module.level * Module.t) list -> summary
(** [bind [level,m;…] def] binds the modules [m] at [level] in [def] *)

val of_partial : Module.Partial.t -> (summary,summary) result
(** Create a summary from a partial module *)
