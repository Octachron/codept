type module_like
type path = Transforms.answer
type query = path Transforms.query_result
type m2l
type state_diff
type state

type path_in_context = {
  loc : Fault.loc;
  edge : Deps.Edge.t option;
  level : Module.level;
  ctx : state_diff;
  path : Paths.Simple.t;
}

val resolve :
  Transforms.param -> state -> path_in_context -> (query, unit) result

val path: query -> path

(** Module like *)
val abstract : module_like
val apply : Transforms.param -> Fault.loc -> module_like  -> module_like
val unpacked : module_like
val fn : module_like Module.Arg.t option -> module_like -> module_like
val ident : path -> module_like
val str : m2l -> module_like
val ext : Transforms.param -> Fault.loc -> string -> unit
val m_with : Paths.Simple.set -> module_like -> module_like
val empty: module_like

(** M2l *)
val m2l_add : state_diff -> m2l -> m2l
val m2l_init : m2l

(** Module rec *)
val bind_rec_add : string -> module_like -> state_diff -> state_diff
val bind_rec_init : state_diff

(** Expr *)
val included : Transforms.param -> Fault.loc -> module_like -> state_diff
val bind : string -> module_like -> state_diff
val bind_sig : string -> module_like -> state_diff
val opened : Transforms.param -> loc:Fault.loc -> module_like -> state_diff
val empty_diff: state_diff


module State: sig
  val merge : state -> state_diff -> state
  val bind_arg : state -> module_like Module.Arg.t -> state
  val is_alias : Transforms.param -> state -> Paths.Simple.t -> bool
  val restart : state -> state_diff -> state
  val bind_alias : state -> string -> Paths.Simple.t -> state_diff
  val diff : state -> state_diff
  val open_path :
    param:Transforms.param -> loc:Fault.loc -> state -> path -> state
end
