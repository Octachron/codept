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


type explanation = string

(** Fault definition *)
type 'data tag


type 'data info = {
  tag: 'data tag;
  path : string list (** hierarchical name for the fault *);
  expl: explanation (** fault documentation *);
  printer: Format.formatter -> 'data -> unit;
}

type 'a printer = Format.formatter -> 'a -> unit

val info: string list -> explanation  -> 'data printer -> 'data info

type fault = Err: 'data tag * 'data -> fault
type value = Fmt: 'data printer * 'data -> value
type t = fault

exception Fatal of value
(** When an error is critic (see {!val:Level.critical}), we raise this exception
    which contains the value that describes the critical problem which its
    {i pretty-printer}. *)

val emit: 'data info -> 'data -> t


(** Basic logging function *)
type log_info = { silent:Level.t; level:Level.t; exit:Level.t}

val log : log_info -> 'a printer -> 'a printer

 (** Fixed level logging functions *)
module Log :
sig
  val critical : 'a printer -> 'a printer
  val error :  'a printer -> 'a printer
  val warning : 'a printer -> 'a printer
  val notification :  'a printer -> 'a printer
  val info : 'a printer -> 'a printer
end


(** Fault handling polycy *)
module Policy :
sig

  type policy
  type t = policy

  val make: silent:Level.t -> exit:Level.t -> t
  val pp: Format.formatter -> t -> unit

  (** {2 Utility functions} *)
  val level : t -> fault -> Level.t
  val set: ?lvl:Level.t -> ?expl:explanation -> string list -> t -> t
  val register: ?lvl:Level.t -> 'a info -> t -> t

  val set_exit: Level.t -> t -> t
  val set_silent: Level.t -> t -> t

end

(** {2 Fault handling with policy} *)
val register : ?lvl:Level.t -> 'a info -> Policy.t -> Policy.t
val handle : Policy.t -> fault -> unit
val raise : Policy.t -> 'a info -> 'a -> unit
val is_silent: Policy.t -> 'a info -> bool
