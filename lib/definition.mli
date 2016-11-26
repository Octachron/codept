(** Short summary of visible and defined modules at an AST node *)

type t = { defined : Module.signature; visible: Module.signature; }
type definition = t
(** Invariant: defined ⊂ visible *)

val pp : Format.formatter -> definition -> unit
(** pretty printer *)

val empty : definition
(** Empty definition *)

val only_visible : definition -> Module.signature
(** List modules that are visibles and not defined *)

val sg_bind : Module.signature -> definition
(** define the given signature *)

val sg_see : Module.signature -> definition
(** make visible the given signature *)

val clear_visible : definition -> definition
(** forget visible but not defined modules *)

(** Basic creation functions *)
module Def :
  sig

    val md : Module.t -> definition
    (** [md m] Define the module [m] *)

    val mods : Module.t list -> definition
    (** [md [m;…]] Define the modules [m;…] *)

    val sg : Module.t -> definition
    (** [sg s] Define the module type [s] *)


    val sgs : Module.t list -> definition
    (** [sg s_list] Define the module types [s_list] *)

    val gen : Module.level -> Module.t -> definition
    (** [gen level m] define the module [m] at [level] *)

    val ( +@ ) : Module.signature -> Module.signature -> Module.signature
    (** merge signatures *)

    val ( +| ) : definition -> definition -> definition
    (** merge definitions *)

  end

(** {2 Basic definition extension} *)

val bind : Module.t -> definition -> definition
(** [bind m def] binds the module m in [def] *)

val see : Module.t -> definition -> definition
(** [bind m def] makes the module [m] visible in [def] *)

val bind_sg : Module.t -> definition -> definition
(** [bind s def] makes the module type [m] visible in [def] *)

val bind_gen : Module.level -> Module.t -> definition -> definition
(** [bind level m def] binds the module [m] at [level] in [def] *)

val binds : (Module.level * Module.t) list -> definition
(** [bind [level,m;…] def] binds the modules [m] at [level] in [def] *)

val of_partial : Module.Partial.t -> definition
(** Create a definition from a partial module *)
