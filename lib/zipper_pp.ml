module Make(Def:Zipper_def.s) = struct
  open Def
  module Sk = Zipper_skeleton
  let path p ppf = Paths.S.pp ppf p
  let pp_path_expr m ppf = Paths.Expr.pp ppf m
  let leaf p = path p.Zipper_skeleton.path
  let (>>) f g = fun ppf -> f ppf; g ppf
  let dlist elt l ppf = Pp.list (fun ppf x -> elt x ppf) ppf l
  let a (x,_) = path x
  let empty _ppf = ()
  let rec zipper (z: Sk.path_in_context zipper)  =
    let f = z.focus in
    let x = leaf f in
    match z.path with
    | Me Ident :: rest -> me rest x
    | Mt Alias :: rest -> mt rest x
    | Access acc :: rest ->
      access rest
        (fun ppf -> Pp.fp ppf "access {%t;%t}" x (dlist a acc.right))
    | Path_expr Main args :: rest ->
      path_expr rest (pp_path_expr {Paths.Expr.path=f.path; args})
    | _ -> assert false
  and me: M2l.module_expr t -> _ = fun rest after ppf -> assert false
  and mt: M2l.module_type t -> _ = fun rest after ppf -> assert false
  and access: waccess t -> _ = fun rest after ppf -> assert false
  and path_expr: Paths.Expr.t t -> _ = fun rest after ppf -> assert false

end
