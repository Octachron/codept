(** Fault definitions and associated polycy *)


module Level :
sig
  type t

  val of_string: string -> t

  val info : t
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

(** Location type for error message *)
type loc = Paths.Pkg.t * Loc.t
val loc: Format.formatter -> loc -> unit

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
  val info :
    ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
end


(** Fault handling polycy *)
module Policy :
sig

  (** {2 Type definition} *)
  type map =
    | Level of {expl: explanation; lvl: Level.t option}
    | Map of {expl:explanation; lvl:Level.t option; map: map Name.map}

  type t = { silent : Level.t; exit : Level.t; map : map; }
  type polycy = t
  val pp: Format.formatter -> t -> unit

  (** {2 Utility functions} *)
  val level : t -> 'a fault -> Level.t
  val register: Paths.S.t * explanation option -> t -> t
  val set : Paths.S.t * explanation option * Level.t -> t -> t
  val set_err : 'a fault * Level.t -> t -> t
  val register_err: 'a fault -> t -> t

end

(** {2 Fault handling with polycy} *)
val set : 'a t * Level.t -> Policy.t -> Policy.t
val handle : Policy.t -> 'a t -> 'a
val is_silent: Policy.t -> 'a fault -> bool
