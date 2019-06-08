module D = Deps
module T = Transforms
let empty = D.empty
let id x = x
let (+) = D.merge

module Pre = struct
  type path = D.t
  type module_expr = D.t
  type access = D.t
  type packed = D.t
  type module_type = D.t
  type m2l = D.t
  type expr = D.t
  type values = D.t
  type annotation = D.t
  type bind_rec = D.t
  type ext = D.t
  type path_expr = D.t
  type path_expr_args = D.t
  type opens = D.t

  let path p = p.T.deps
  let abstract = empty
  let access = id
  let access_add d _ _ d' = d + d'
  let access_init = empty
  let add_packed _ x y = x + y
  let alias = id
  let annot x y z = x + y + z
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
  let minor = id
  let mt_ext ~loc:_ _ = id
  let mt_fun arg y = match arg with
    | None -> y
    | Some {Module.Arg.signature; _ }  -> signature + y
  let me_fun = mt_fun
  let mt_ident = id
  let mt_of = id
  let mt_sig = id
  let mt_with x _ y = x + y
  let open_add x y = x + y
  let open_init = empty
  let open_me x y = x + y
  let packed_int = empty
  let path_expr x y = x + y
  let path_expr_arg _ x y = x + y
  let path_expr_arg_init = empty
  let sig_abstract = empty
  let str = id
  let sig_include ~loc:_ = id
  let unpacked = empty
  let packed_init = empty
  let value_add x y = x + y
  let value_init = empty
  let values = id
 end

module Outline(Env:Outliner.envt) = M2l_fold.Make(Pre)(Env)

module Default = Outline(Envt.Core)

module Make(Env:Outliner.envt)(Param:Outliner.param):
  Outliner.s with type envt := Env.t =
struct
  let param = let open Param in
    { Transforms.transparent_aliases; policy;
      epsilon_dependencies; transparent_extension_nodes
    }
  include Outline(Env)
  let adapt f ~pkg = f ~pkg param
  let next = adapt next
end
