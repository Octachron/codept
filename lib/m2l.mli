(**
   The M2l module (for Module Meta Language) defines the codept AST.

   This AST is a restriction of the Ocaml parsetree AST, that can
   describe all constructions at the module level but have a very
   simplified view of value (and type) level expressions.

   For instance,
   {[
     module A = struct
       module Inner = struct let f x = x end
     end
     let x = 1
     open A
     module B = struct let y = Inner.f x end
   ]}
   is translated to
   {[
     module A = struct
       module Inner = struct end
     end
     open A
     module B = struct [%access {Inner}]  end
   ]}

*)



(** {2 Main types } *)

type 'a bind = { name: Name.t; expr:'a }
(** ['a bind] is an auxilliary type used for module/module type binding *)

type kind = Structure | Signature
(** The nature (ml/mli) of an m2l ast *)

(** An m2l ast/code fragment is a list of module level expression *)
type m2l = expression Loc.ext list

(** The [expression] type is the basic building block of the m2l AST *)
and expression =
  | Defs of Summary.t (** Resolved module actions M = … / include … / open … *)
  | Open of module_expr (** [open A.B.C] ⇒ [Open [A;B;C]]  *)
  | Include of module_expr (** [struct include A end] *)
  | SigInclude of module_type
  (** [struct include A end] *)
  | Bind of module_expr bind
  (** [struct module A = struct … end] *)
  | Bind_sig of module_type bind   (** [struct module type A = sig … end] *)
  | Bind_rec of module_expr bind list
  (** [module rec A = … and B = … and …] *)
  | Minor of annotation
  (** value level expression.
      Invariant: for any pure outliner [f], [ f (Minor m :: q ) ≡ f q ],
      i.e, this expression constructor is only meaningful for dependencies
      tracking.
  *)
  | Extension_node of extension
  (** [[%ext …]] *)

(** An annotation represents a short value of type or value level
    constructions, that contains only the information relevant for
    dependency tracking.
*)
and annotation =
  { access: access
  (** [M.N.L.x] ⇒ access \{M.N.L = Normal \}
      type t = A.t ⇒ access \{ A = ε \}
  *)
  ; values: m2l list (** − [let open A in …] ⇒ [[ Open A; …  ]]
                         − [let module M = … ] ⇒ [[ Include A; … ]]
                     *)
  ; packed: module_expr Loc.ext list (** [(module M)] *)
  }
and access = (Loc.t * Deps.Edge.t) Paths.S.map

(** Module level expression representation *)
and module_expr =
  | Resolved of Module.Partial.t
  (** Already resolved module expression, generally
      used in subexpression when waiting for other parts
      of module expression to be resolved
  *)
  | Ident of Paths.Simple.t (** [A.B…] **)
  | Apply of {f: module_expr; x:module_expr} (** [F(X)] *)
  | Fun of module_expr fn (** [functor (X:S) -> M] *)
  | Constraint of module_expr * module_type (** [M:S] *)
  | Str of m2l (** [struct … end] *)
  | Val of annotation (** [val … ] *)
  | Extension_node of extension (** [[%ext …]] *)
  | Abstract
  (** empty module expression, used as a placeholder.
      In particular, it is useful for constraining first class module unpacking
      as [Constraint(Abstract, signature)]. *)
  | Unpacked (** [(module M)] *)
  | Open_me of { resolved: Summary.t; opens:Paths.Simple.t list; expr:module_expr}
  (** [M.(…N.( module_expr)…)]
      Note: This construction does not exist (yet?) in OCaml proper.
      It is used here to simplify the interaction between
      pattern open and first class module.*)

(** Module type level representation *)
and module_type =
  | Resolved of Module.Partial.t (** same as in the module type *)
  | Alias of Paths.Simple.t (** [module m = A…]  *)
  | Ident of Paths.Expr.t
  (** [module M : F(X).s]
      Note: Paths.Expr is used due to [F.(X).s] expressions
      that do not have an equivalent on the module level
 *)
  | Sig of m2l (** [sig … end] *)
  | Fun of module_type fn (** [functor (X:S) → M] *)
  | With of {
      body: module_type;
      deletions: Paths.S.set;
      access:access
      (** equalities: type t= A.M.x N.y -> {A.M; N} *)
    }
  (** [S with module N := …]
      we are only tracking module level modification
  *)
  | Of of module_expr (** [module type of …] *)
  | Extension_node of extension (** [%%… ] *)
  | Abstract (** placeholder *)

(** Functor auxiliary type *)
and 'a fn = { arg: module_type Module.Arg.t option; body:'a }


(** An specific represention for an extension node *)
and extension = {name:string; extension:extension_core}

(** A type for extension payload *)
and extension_core =
  | Module of m2l
  | Val of annotation


type t = m2l

(** {2 Schematic serialization } *)
val sch: m2l Schematic.t
module Sch: sig
  val expr: expression Schematic.t
  val module_expr: module_expr Schematic.t
  val module_type: module_type Schematic.t
  val annotation: annotation Schematic.t
end

module Annot : sig
  type t = annotation Loc.ext

  module Access: sig
    type t = access
    val empty: t
    val merge: t -> t -> t
  end

  val empty: t
  val is_empty: t -> bool

  val merge: t -> t -> t

  val (++): t -> t -> t
  (** [ (++) ≡ merge ]*)

  val union: t list -> t
  val union_map: ('a -> t) -> 'a list -> t

  val access: Paths.S.t Loc.ext -> t
  val abbrev:  Paths.S.t Loc.ext -> t
  val value: m2l list -> t
  val pack: module_expr Loc.ext list Loc.ext -> t
  val opt: ('a -> t) -> 'a option -> t
  val epsilon_promote: t -> t
end

module Build: sig
  val ghost: expression -> expression Loc.ext
  val access: Paths.Expr.t Loc.ext -> expression Loc.ext
  val open_path: Paths.Simple.t Loc.ext -> expression Loc.ext
  val open_: module_expr Loc.ext -> expression Loc.ext
  val value: m2l list -> expression Loc.ext
  val pack: module_expr Loc.ext list Loc.ext -> expression Loc.ext

  val open_me: Paths.Simple.t list -> module_expr -> module_expr

  val demote_str: module_expr fn -> Summary.t Module.Arg.t option
    -> module_expr fn
  val demote_sig: module_type fn -> Summary.t Module.Arg.t option
    -> module_type fn

  val fn_sig: module_type fn -> module_type
  val fn: module_expr fn -> module_expr
end


(**
   {2 Basis analysis }

*)

(**
   The Block module gathers functions that aims to compute the first
   dependencies that need to be resolved before any outliner can make
   progress evaluating a given code block *)
module Block: sig
  val m2l: m2l -> (Summary.t * Paths.S.t) Loc.ext option
end

(** {!Normalize} computes the normal form of a given m2l code fragment.
    When possible, successive expression of the same kind are merged.
 *)
module Normalize: sig

  val all: m2l -> bool * m2l
  (** [all fragment ≡ (has_some_simplification_been_made, resulting_m2l) ] *)

  val minor: Annot.t -> Annot.t
  val value: Annot.t -> m2l -> Annot.t
end

(** {2 Signature filter} *)
module Sig_only: sig
  val filter: m2l -> m2l
end


(** {2 Printers } *)

val pp: Format.formatter -> m2l -> unit
val pp_expression: Format.formatter -> expression -> unit
val pp_annot: Format.formatter -> annotation -> unit
val pp_me: Format.formatter -> module_expr -> unit
val pp_mt: Format.formatter -> module_type -> unit
val pp_access: access Pp.t
val pp_packed: module_expr Loc.ext list Pp.t
val pp_values: m2l list Pp.t
