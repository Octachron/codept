
type ('a,'b) pair = { backbone:'a; user:'b }

type a = Paths.Simple.t * (Loc.t * Deps.Edge.t)
module Arg = Module.Arg


module type tree = sig
  type path
  type module_expr
  type access
  type packed
  type module_type
  type m2l
  type expr
  type values
  type annotation
  type bind_rec
  type ext
  type path_expr
  type path_expr_args
  type opens
end


module type fold = sig
  include tree
  val path : Zipper_skeleton.query -> path
  val abstract : module_expr
  val access :  access -> access
  val access_add :
    path -> Fault.loc -> Deps.Edge.t -> access -> access
  val access_init : access
  val add_packed :
    Fault.loc -> module_expr -> packed -> packed
  val alias : path -> module_type
  val annot : packed -> access -> values -> annotation

  val apply : Fault.loc -> module_expr -> module_expr -> module_expr

  val bind : Name.t -> module_expr -> expr
  val bind_alias: Name.t -> Paths.S.t -> expr
  val bind_rec : bind_rec -> expr
  val bind_rec_add :
    string -> module_expr -> bind_rec -> bind_rec
  val bind_rec_init : bind_rec
  val bind_sig : string -> module_type -> expr

  val expr_ext : string -> ext -> expr
  val expr_include : loc:Fault.loc -> module_expr -> expr
  val expr_open : loc:Fault.loc -> module_expr -> expr
  val ext_module : m2l -> ext
  val ext_val : annotation -> ext

  val m2l_add : Fault.loc -> expr -> m2l -> m2l
  val m2l_init : m2l
  val m2l: m2l  -> m2l

  val me_constraint : module_expr -> module_type -> module_expr
  val me_ext : loc:Fault.loc -> string -> ext -> module_expr
  val me_fun :
    module_type Arg.t option -> module_expr -> module_expr
  val me_ident : path -> module_expr
  val me_val : annotation -> module_expr
  val minor : annotation -> expr

  val mt_ext : loc:Fault.loc -> string -> ext -> module_type
  val mt_fun :
    module_type Arg.t option -> module_type -> module_type
  val mt_ident : path_expr -> module_type
  val mt_of : module_expr -> module_type
  val mt_sig : m2l -> module_type
  val mt_with :
    access -> Paths.Simple.set -> module_type -> module_type
  val open_add :
    path -> opens -> opens
  val open_init : opens
  val open_me : opens -> module_expr -> module_expr

  val packed_init : packed

  val path_expr_pure : path -> path_expr
  val path_expr_app : path_expr -> path_expr -> Paths.S.t option -> path_expr
  val sig_abstract : module_type
  val sig_include : loc:Fault.loc -> module_type -> expr
  val str : m2l -> module_expr
  val unpacked : module_expr

  val value_add : m2l -> values -> values
  val value_init : values
  val values : values -> values

end

module type s = sig
  module T : tree
  module Abbrevs: sig
    type path = (Zipper_skeleton.path, T.path) pair
    type module_expr = (Zipper_skeleton.module_like, T.module_expr) pair
    type access = T.access
    type packed = T.packed
    type module_type = (Zipper_skeleton.module_like, T.module_type) pair
    type m2l = (Zipper_skeleton.m2l, T.m2l) pair
    type values = T.values
    type bind_rec = (Zipper_skeleton.state_diff, T.bind_rec) pair
    type path_expr_t = (Zipper_skeleton.path, T.path_expr) pair
    type opens = T.opens
    type path_in_context = Zipper_skeleton.path_in_context
  end
  open Abbrevs


  type waccess = W of access [@@unboxed]

  type 'focus expr =
    | Open: M2l.module_expr expr
    | Include:  M2l.module_expr expr
    | SigInclude:  M2l.module_type expr
    | Bind: Name.t ->  M2l.module_expr expr
    | Bind_sig: Name.t -> M2l.module_type expr
    | Bind_rec_sig:
        {
          diff: Zipper_skeleton.state_diff;
          left: (Name.t * T.module_type * M2l.module_expr) list;
          name: Name.t;
          expr: M2l.module_expr;
          right: M2l.module_expr M2l.bind list
        } -> M2l.module_type expr
    | Bind_rec:
        {
          left: bind_rec;
          name:Name.t;
          mt: T.module_type;
          right: (Name.t * T.module_type * M2l.module_expr) list;
        } -> M2l.module_expr expr
    | Minor:  M2l.annotation expr
    | Extension_node: string ->  M2l.extension_core expr

  type 'focus annot =
    | Packed: {
        left:packed;
        loc:Fault.loc;
        right: M2l.module_expr Loc.ext list;
        access: M2l.access;
        values: M2l.m2l list }
        -> M2l.module_expr annot
    | Access :
        {
          packed: packed;
          values: M2l.m2l list
        }
        -> waccess annot
    | Values:
        { packed: packed;
          access: access;
          left: values;
          right: M2l.m2l list
        }
        -> M2l.m2l annot

  type acc =
    {left: access;
     right:a list
    }

  type  'f path_expr =
    | Simple: path_in_context path_expr
    | App_f: Paths.Expr.t * Paths.S.t option -> Paths.Expr.t path_expr
    | App_x: path_expr_t * Paths.S.t option -> Paths.Expr.t path_expr

  type 'focus me =
    | Ident: path_in_context me
    | Apply_left: M2l.module_expr -> M2l.module_expr me
    | Apply_right: module_expr -> M2l.module_expr me
    | Fun_left: {name:string; body:M2l.module_expr} -> M2l.module_type me
    | Fun_right:
        (module_type Arg.t * Zipper_skeleton.state_diff ) option
        -> M2l.module_expr me
    | Constraint_left: M2l.module_type -> M2l.module_expr me
    | Constraint_right: module_expr -> M2l.module_type me
    | Str: M2l.m2l me
    | Val: M2l.annotation me
    | Extension_node: string -> M2l.extension_core me
    | Open_me_left:
        { left: opens;
          right:Paths.S.t list;
          diff:Zipper_skeleton.state_diff;
          expr:M2l.module_expr
        } -> path_in_context me
    | Open_me_right:
        {opens:opens; state:Zipper_skeleton.state_diff} -> M2l.module_expr me

  type 'focus mt =
    | Alias: path_in_context mt
    | Ident: Paths.Expr.t mt
    | Sig: M2l.m2l mt
    | Fun_left: {name:string; body:M2l.module_type} -> M2l.module_type mt
    | Fun_right: (module_type Arg.t * Zipper_skeleton.state_diff) option
        -> M2l.module_type mt
    | With_access:
        {body:M2l.module_type; deletions: Paths.S.set} -> waccess mt
    | With_body:
        {access:access; deletions:Paths.S.set } -> M2l.module_type mt
    | Of: M2l.module_expr mt
    | Extension_node: string -> M2l.extension_core mt

  type 'focus ext =
    | Mod: M2l.m2l ext
    | Val: M2l.annotation ext

  type ('elt,'from) elt =
    | M2l: {left:m2l;
            loc:Fault.loc;
            state:Zipper_skeleton.state_diff;
            right:M2l.m2l}
        -> (M2l.expression, M2l.m2l) elt
    | Expr: 'elt expr -> ('elt,M2l.expression) elt
    | Annot: 'elt annot -> ('elt, M2l.annotation) elt
    | Me: 'elt me -> ('elt, M2l.module_expr) elt
    | Mt: 'elt mt -> ('elt, M2l.module_type) elt
    | Access: acc -> (path_in_context, waccess) elt
    | Ext: 'elt ext ->  ('elt, M2l.extension_core) elt
    | Path_expr: 'elt path_expr -> ('elt, Paths.Expr.t) elt

  type 'f t =
    | []: M2l.m2l t
    | (::): ('focus,'from) elt * 'from t -> 'focus t

  type 'result zipper = { path: 'result t; focus: 'result }

end
