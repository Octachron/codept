(** Fault definitions and associated polycy *)


module Level :
sig
  type t
  val of_string: string -> t
  val of_int: int -> t


  val whisper : t
  (** Minor fault that should not deserve attention in standard setting *)

  val notification : t
  (** Minor fault that ought to be notified *)

  val warning : t
  (** Intermediary warning that should be highly visible to user *)

  val error : t
  (** Fault that endanger the correctness of the program *)

  val critical : t
  (** Non recoverable error *)

end

(** Contextual information for logger *)
type log_info = {
  silent : Level.t (** level below which no message should be logged *);
  level : Level.t (** level of the fault being logged *);
  exit : Level.t (** level beyond which the logger should exit after logging *); }



type explanation = string

(** Fault definition *)
type 'a fault = { path : Paths.S.t (** hierarchical name for the fault *);
                  expl: explanation (** fault documentation *);
                  log : log_info -> 'a (** logging function*); }

type 'a t = 'a fault (**type synonym *)

(** Basic logging function *)
val log : log_info -> ('a, Format.formatter, unit) format -> 'a


 (** Fixed level logging functions *)
module Log :
sig
  val critical : ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a
  val error : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
  val warning :
    ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
  val notification :
    ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
  val whisper :
    ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
end

(** {2 Predefined faults} *)

(** Location type for error message *)
type loc = Paths.Pkg.t * M2l.Loc.t
val loc: Format.formatter -> loc -> unit

(** {3 Extension node fault} *)
val extension_ignored : ( loc -> string -> unit) t
val extension_traversed : (loc -> string -> unit) t

(** {3 First-class module faults} *)
val opened_first_class : (loc -> string -> unit) t
val included_first_class : (loc -> unit) t

(** {3 Typing fault} *)
val applied_structure : (loc -> Module.Partial.t -> unit) t
val structure_expected : (loc -> Module.Partial.t -> unit) t
val applied_unknown : (loc -> Module.Partial.t -> unit) t
val unknown_approximated : (Paths.S.t -> loc -> unit) t

(** {3 Parsing approximation faults} *)
val concordant_approximation : (Paths.Pkg.t -> unit) t
val discordant_approximation :
  (Paths.Pkg.t -> string list -> string list -> unit) t
val syntaxerr : (Syntaxerr.error -> unit) t

(** Fault handling polycy *)
module Polycy :
sig

  (** {2 Type definition} *)
  type map =
    | Level of {expl: explanation; lvl: Level.t option}
    | Map of {expl:explanation; lvl:Level.t option; map: map Name.map}

  type t = { silent : Level.t; exit : Level.t; map : map; }
  type polycy = t
  val pp: Format.formatter -> t -> unit

  (** {2 Utility functions} *)
  val find : t -> Paths.S.t -> Level.t
  val set : Paths.S.t * explanation option * Level.t -> t -> t
  val set_err : 'a fault * Level.t -> t -> t

  (** {2 predefined polycy } *)
  val strict : t
  val default : t
  val parsing_approx : t
  val lax : t
  val quiet : t
end

(** {2 Fault handling with polycy} *)
val set : 'a t * Level.t -> Polycy.t -> Polycy.t
val handle : Polycy.t -> 'a t -> 'a
