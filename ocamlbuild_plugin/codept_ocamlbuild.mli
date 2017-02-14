(** Codept plugin for ocamlbuild *)

(** Initialize codept rules *)
val init: unit -> unit

(** Initialize codept rules during the after rule hook *)
val handler: Ocamlbuild_plugin.hook -> unit

(** [dispatch h] expand codept handler with h *)
val expand_dispatch: (Ocamlbuild_plugin.hook -> unit) -> unit

(** Dispatch codept extended rules *)
val dispatch: unit -> unit
