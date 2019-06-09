module type fold = sig
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

  val path : M2l_skel.query -> path
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
    module_type Module.Arg.t option -> module_expr -> module_expr
  val me_ident : path -> module_expr
  val me_val : annotation -> module_expr
  val minor : annotation -> expr

  val mt_ext : loc:Fault.loc -> string -> ext -> module_type
  val mt_fun :
    module_type Module.Arg.t option -> module_type -> module_type
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

  val path_expr :
    path -> path_expr_args -> path_expr
  val path_expr_arg :
    int -> path_expr -> path_expr_args -> path_expr_args
  val path_expr_arg_init : path_expr_args
  val sig_abstract : module_type
  val sig_include : loc:Fault.loc -> module_type -> expr
  val str : m2l -> module_expr
  val unpacked : module_expr

  val value_add : m2l -> values -> values
  val value_init : values
  val values : values -> values

end

module Make(X:fold)(Env:Stage.envt):
  Stage.generic_outliner with
  type envt := Env.t and type final := X.m2l
  and type 'a with_param := 'a Stage.param
