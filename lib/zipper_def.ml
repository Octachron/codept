
type ('a,'b) pair = { backbone:'a; user:'b }

type a = Paths.Expr.t * (Loc.t * Deps.Edge.t)
module Arg = Module.Arg


module type tree = sig
  type path
  type module_expr
  type access
  type minor
  type minors
  type module_type
  type with_constraints
  type m2l
  type expr
  type bind_rec
  type ext
  type path_expr
  type path_expr_args
  type opens
end

type state_diff = Zipper_skeleton.state_diff

module type fold = sig
  include tree
  val path : Zipper_skeleton.query -> path
  val abstract : module_expr
  val alias : path -> module_type

  val access_add :
    path_expr -> Uloc.t -> Deps.Edge.t -> access -> access
  val access_init : access
  val access : access -> minor
  val pack: module_expr -> minor
  val minor_ext: loc:Uloc.t -> string -> ext -> minor
  val local_open: module_expr -> minors -> minor
  val local_bind: Name.t option -> module_expr -> minors -> minor
  val empty_minors: minors
  val add_minor: minor -> minors -> minors

  val apply : Uloc.t -> module_expr -> module_expr -> module_expr

  val bind : Name.t option -> module_expr -> expr
  val bind_alias: Name.t option -> Paths.S.t -> expr
  val bind_rec : bind_rec -> expr
  val bind_rec_add :
    Name.t option -> module_expr -> bind_rec -> bind_rec
  val bind_rec_init : bind_rec
  val bind_sig : Name.t option -> module_type -> expr

  val expr_ext : string -> ext -> expr
  val expr_include : loc:Uloc.t -> module_expr -> expr
  val expr_open : loc:Uloc.t -> module_expr -> expr
  val ext_module : m2l -> ext
  val ext_val : minors -> ext

  val m2l_add : Uloc.t -> expr -> m2l -> m2l
  val m2l_init : m2l
  val m2l: m2l  -> m2l

  val me_constraint : module_expr -> module_type -> module_expr
  val me_ext : loc:Uloc.t -> string -> ext -> module_expr
  val me_fun :
    module_type Arg.t option -> module_expr -> module_expr
  val me_ident : path -> module_expr
  val me_val : minors -> module_expr
  val minor : minors -> expr

  val mt_ext : loc:Uloc.t -> string -> ext -> module_type
  val mt_fun :
    module_type Arg.t option -> module_type -> module_type
  val mt_ident : path_expr -> module_type
  val mt_of : module_expr -> module_type
  val mt_sig : m2l -> module_type
  val mt_with : module_type -> with_constraints -> module_type

  val with_init: with_constraints
  val with_type: minors -> with_constraints -> with_constraints
  val with_module:
    delete:bool -> lhs:Paths.S.t -> rhs:module_expr -> with_constraints -> with_constraints
  val with_module_type:
    delete:bool -> lhs:Paths.S.t -> rhs:module_type -> with_constraints -> with_constraints

  val open_add :
    path -> opens -> opens
  val open_init : opens
  val open_me : opens -> module_expr -> module_expr

  val path_expr_pure : path -> path_expr
  val path_expr_app : path_expr -> path_expr -> path_expr
  val path_expr_proj: path_expr -> Paths.S.t -> path -> path_expr

  val sig_abstract : module_type
  val sig_include : loc:Uloc.t -> module_type -> expr
  val str : m2l -> module_expr
  val unpacked : module_expr

end

module type s = sig
  module T : tree
  module Abbrevs: sig
    type path = (Zipper_skeleton.path, T.path) pair
    type module_expr = (Zipper_skeleton.module_like, T.module_expr) pair
    type access = T.access
    type minor = T.minor
    type minors = T.minors
    type module_type = (Zipper_skeleton.module_like, T.module_type) pair
    type with_constraints = (Zipper_skeleton.module_like, T.with_constraints) pair
    type m2l = (Zipper_skeleton.m2l, T.m2l) pair
    type bind_rec = (state_diff, T.bind_rec) pair
    type path_expr_t = (Zipper_skeleton.module_like, T.path_expr) pair
    type opens = T.opens
    type path_in_context = Zipper_skeleton.path_in_context
  end
  open Abbrevs


  type waccess = W of access [@@unboxed]

  type 'focus expr =
    | Open: M2l.module_expr expr
    | Include:  M2l.module_expr expr
    | SigInclude:  M2l.module_type expr
    | Bind: Name.t option ->  M2l.module_expr expr
    | Bind_sig: Name.t option -> M2l.module_type expr
    | Bind_rec_sig:
        {
          diff: state_diff;
          left: (Name.t option * T.module_type * M2l.module_expr) list;
          name: Name.t option;
          expr: M2l.module_expr;
          right: M2l.module_expr M2l.bind list
        } -> M2l.module_type expr
    | Bind_rec:
        {
          left: bind_rec;
          name:Name.t option;
          mt: T.module_type;
          right: (Name.t option * T.module_type * M2l.module_expr) list;
        } -> M2l.module_expr expr
    | Minors: M2l.minor list expr
    | Extension_node: string ->  M2l.extension_core expr

  type 'focus minor =
    | Access : waccess minor
    | Pack : M2l.module_expr minor
    | Extension_node : Name.t -> M2l.extension_core minor

    | Local_open_left : state_diff * Loc.t * M2l.minor list -> M2l.module_expr minor
    | Local_open_right: state_diff * module_expr -> M2l.minor list minor

    | Local_bind_left: state_diff * Name.t option * M2l.minor list -> M2l.module_expr minor
    | Local_bind_right: state_diff * Name.t option * module_expr -> M2l.minor list minor

  type acc =
    {left: access;
     right:a list
    }

  type  'f path_expr =
    | Simple: path_in_context path_expr
    | App_f: Paths.Expr.t * (Module.level * Deps.Edge.t * Paths.S.t) option -> Paths.Expr.t path_expr
    | App_x: path_expr_t * (Module.level * Deps.Edge.t * Paths.S.t) option -> Paths.Expr.t path_expr
    | Proj: path_expr_t * Paths.S.t -> path_in_context path_expr

  type 'focus me =
    | Ident: path_in_context me
    | Apply_left: M2l.module_expr -> M2l.module_expr me
    | Apply_right: module_expr -> M2l.module_expr me
    | Fun_left: {name:Name.t option; diff:state_diff; body:M2l.module_expr} -> M2l.module_type me
    | Fun_right:
        (module_type Arg.t * state_diff ) option
        -> M2l.module_expr me
    | Constraint_left: M2l.module_type -> M2l.module_expr me
    | Constraint_right: module_expr -> M2l.module_type me
    | Str: M2l.m2l me
    | Val: M2l.minor list me
    | Extension_node: string -> M2l.extension_core me
    | Open_me_left:
        { left: opens;
          right:Paths.S.t Loc.ext list;
          diff:state_diff;
          loc: Loc.t;
          expr:M2l.module_expr
        } -> path_in_context me
    | Open_me_right:
        {opens:opens; state:state_diff} -> M2l.module_expr me

  type 'focus mt =
    | Alias: path_in_context mt
    | Ident: Paths.Expr.t mt
    | Sig: M2l.m2l mt
    | Fun_left: {name:Name.t option; diff:state_diff; body:M2l.module_type} -> M2l.module_type mt
    | Fun_right: (module_type Arg.t * state_diff) option
        -> M2l.module_type mt
    | With_constraints: {
        original_body:module_type;
        right:M2l.with_constraint list
      }
        -> M2l.with_constraint mt
    | With_body: M2l.with_constraint list -> M2l.module_type mt
    | Of: M2l.module_expr mt
    | Extension_node: string -> M2l.extension_core mt

  type 'focus with_constraint =
    | With_type: with_constraints ->  M2l.minor list with_constraint
    | With_module: {body:with_constraints; lhs:Paths.S.t; delete:bool} -> path_in_context with_constraint
    | With_module_type: {body:with_constraints; lhs:Paths.S.t; delete:bool} -> M2l.module_type with_constraint



  type 'focus ext =
    | Mod: M2l.m2l ext
    | Val: M2l.minor list ext

  type ('elt,'from) elt =
    | M2l: {left:m2l;
            loc:Uloc.t;
            state:state_diff;
            right:M2l.m2l}
        -> (M2l.expression, M2l.m2l) elt
    | Expr: 'elt expr -> ('elt,M2l.expression) elt
    | Minor: 'elt minor -> ('elt, M2l.minor) elt
    | Minors:
        { left: minors; right: M2l.minor list } ->
        (M2l.minor,M2l.minor list) elt
    | Me: 'elt me -> ('elt, M2l.module_expr) elt
    | Mt: 'elt mt -> ('elt, M2l.module_type) elt
    | With_constraint: 'elt with_constraint -> ('elt, M2l.with_constraint) elt
    | Access: acc -> (Paths.Expr.t, waccess) elt
    | Ext: 'elt ext ->  ('elt, M2l.extension_core) elt
    | Path_expr: 'elt path_expr -> ('elt, Paths.Expr.t) elt

  type 'f t =
    | []: M2l.m2l t
    | (::): ('focus,'from) elt * 'from t -> 'focus t

  type 'result zipper = { path: 'result t; focus: 'result }

end
