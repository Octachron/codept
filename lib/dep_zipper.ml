module D = Deps
module T = Transforms
let empty = D.empty
let id x = x
let (+) = D.merge

module Pre = struct
  type path = D.t
  type module_expr = D.t
  type access = D.t
  type minor = D.t
  type minors = D.t
  type module_type = D.t
  type m2l = D.t
  type expr = D.t
  type bind_rec = D.t
  type ext = D.t
  type path_expr = D.t
  type path_expr_args = D.t
  type opens = D.t
  type with_constraints = D.t

  let path p = p.T.deps
  let abstract = empty
  let access = id
  let access_add d _ _ d' = d + d'
  let access_init = empty
  let alias = id

  let pack x = x
  let empty_minors = empty
  let add_minor x y = x + y
  let local_open x y = x + y
  let local_bind _ x y = x + y
  let minor_ext ~loc:_ _ x = x
  let apply _ x y = x + y
  let bind _ = id
  let bind_alias _ _ = empty
  let bind_sig _ = id
  let bind_rec = id
  let bind_rec_init = empty
  let bind_rec_add _ x y = x + y
  let expr_ext _ = id
  let expr_include ~loc:_ = id
  let expr_open ~loc:_ = id
  let ext_module = id
  let ext_val = id
  let m2l = id
  let m2l_init = empty
  let m2l_add _ x y = x + y
  let me_constraint x y = x + y
  let me_ext ~loc:_ _ = id
  let me_ident = id
  let me_val = id
  let me_proj me _proj res = me + res
  let minor = id
  let mt_ext ~loc:_ _ = id
  let mt_fun arg y = match arg with
    | None -> y
    | Some {Module.Arg.signature; _ }  -> signature + y
  let me_fun = mt_fun
  let mt_ident = id
  let mt_of = id
  let mt_sig = id
  let mt_with = (+)
  let with_type= (+)
  let with_lhs = (+)
  let with_module_type ~delete:_ ~lhs:_ ~rhs:x y = x + y
  let with_module ~delete:_ ~lhs:_ ~rhs:x y = x + y
  let with_init = empty
  let open_add x y = x + y
  let open_init = empty
  let open_me x y = x + y
  let path_expr_pure x = x
  let path_expr_app x y = x + y
  let path_expr_proj x _ y = x + y
  let sig_abstract = empty
  let str = id
  let sig_include ~loc:_ = id
  let unpacked = empty
 end

module Outline(Env:Stage.envt) = Zipper.Fold.Make(Pre)(Env)

module Make(Env:Stage.envt)(Param:Stage.param):
  Stage.outliner with type envt := Env.t =
struct
  let param = let open Param in
    { Transforms.transparent_aliases; fault_handler;
      epsilon_dependencies; transparent_extension_nodes
    }
  include Outline(Env)
  let adapt f ~pkg = f ~pkg param
  let next = adapt next
end
