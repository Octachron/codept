module Make(Def:Zipper_def.s) = struct
  open Def
  module Sk = Zipper_skeleton
  let path p ppf = Paths.S.pp ppf p
  let pp_path_expr m ppf = Format.fprintf ppf "%a?" Paths.Expr.pp m
  let leaf p ppf = Format.fprintf ppf "%t?" (path p.Zipper_skeleton.path)
  let dlist elt l ppf = Pp.list (fun ppf x -> elt x ppf) ppf l
  let a (x,_) = path x
  let rec zipper (z: Sk.path_in_context zipper)  =
    let f = z.focus in
    let x = leaf f in
    match z.path with
    | Me Ident :: rest -> me rest x
    | Mt Alias :: rest -> mt rest x
    | Access acc :: rest ->
      access rest
        (fun ppf -> Pp.fp ppf "access {%t;...%t}" x (dlist a acc.right))
    | Path_expr Main args :: rest ->
      path_expr rest (pp_path_expr {Paths.Expr.path=f.path; args})
    | Me (Open_me_left {right;expr; _}) :: rest ->
      me rest
        (fun ppf ->
           Pp.fp ppf "%t.(%t.(%a))" x (dlist path right) M2l.pp_me expr
        )
    | _ -> .
  and me: M2l.module_expr t -> _ = fun rest sub ->
    match rest with
    | Me (Apply_left right) :: rest ->
      me rest (fun ppf -> Pp.fp ppf "%t(%a)" sub M2l.pp_me right)
    | Me Apply_right _ :: rest ->
      me rest (fun ppf -> Pp.fp ppf "?(%t)" sub)
    | Me Fun_right _ :: rest ->
      me rest (fun ppf -> Pp.fp ppf "fun (?) ->%t" sub)
    | Me Constraint_left mt :: rest ->
      me rest (fun ppf -> Pp.fp ppf "(%t:%a)" sub M2l.pp_mt mt)
    | Me Open_me_right _ :: rest ->
      me rest (fun ppf -> Pp.fp ppf "?.(%t)" sub)
    | Expr Include :: rest ->
      expr rest (fun ppf -> Pp.fp ppf "include %t" sub)
    | Expr Bind name :: rest ->
      expr rest (fun ppf -> Pp.fp ppf "module %s=%t" name sub)
    | Expr Bind_rec m :: rest ->
      expr rest (fun ppf ->
          Pp.fp ppf "@[module rec %s: ? =%t@,%a@]" m.name sub
            (Pp.list (fun ppf (name,_,me) ->
                 Pp.fp ppf "@,and %s:?=%a" name M2l.pp_me me)
            ) m.right
        )
    | Expr Open :: rest ->
      expr rest (fun ppf -> Pp.fp ppf "open %t" sub)
    | Annot Packed p :: rest ->
      annot rest (fun ppf -> Pp.fp ppf
                     "@[Annot@ packed:%t ... {%a%a%a}@]"
                     sub M2l.pp_packed p.right
                     M2l.pp_access p.access
                     M2l.pp_values p.values
                 )
    | Mt Of :: rest ->
      mt rest (fun ppf -> Pp.fp ppf "module type of %t" sub)
    | _ -> .
  and annot: M2l.annotation t -> _ = fun rest sub -> match rest with
    | Expr Minor :: rest -> expr rest sub
    | Me Val :: rest -> me rest (fun ppf ->
        Pp.fp ppf "(val %t)" sub
      )
    | Ext Val :: rest -> ext rest sub
    | _ -> .
  and mt: M2l.module_type t -> _ = fun rest sub -> match rest with
    | Expr Bind_sig name :: rest ->
      expr rest (fun ppf -> Pp.fp ppf "module type %s=%t" name sub)
    | Me Fun_left {name;body} :: rest ->
      me rest
        (fun ppf -> Pp.fp ppf "fun(%s:%t)->%a" name sub M2l.pp_me body)
    | Mt Fun_left {name;body} :: rest ->
      mt rest
        (fun ppf -> Pp.fp ppf "fun(%s:%t)->%a" name sub M2l.pp_mt body)
    | Mt Fun_right _ :: rest ->
      mt rest
        (fun ppf -> Pp.fp ppf "fun(?)->%t" sub)
    | Expr SigInclude :: rest ->
      expr rest (fun ppf -> Pp.fp ppf "include %t" sub)
    | Expr Bind_rec_sig m :: rest ->
      expr rest (fun ppf ->
          Pp.fp ppf "module rec %a %s:%t=%a@,%a"
            (Pp.list (fun ppf (name,_,me) ->
                 Pp.fp ppf "and@ %s:?=%a" name M2l.pp_me me)
            ) m.left
            m.name sub M2l.pp_me m.expr
            (Pp.list (fun ppf {M2l.name;expr} ->
                 Pp.fp ppf "and@ %s = %a" name M2l.pp_me expr)
            ) m.right
        )
    | Me Constraint_right _ :: rest ->
      me rest (fun ppf -> Pp.fp ppf "(?:%t)" sub)
    | Mt With_body m :: rest ->
      mt rest (fun ppf -> Pp.fp ppf "%t without {%a}" sub
                  (Pp.list Paths.S.pp) (Paths.S.Set.elements m.deletions)
              )
    | _ -> .
  and expr: M2l.expression t -> _ = fun rest sub -> match rest with
    | M2l m :: _ ->
      fun ppf -> Pp.fp ppf "%t@ %a" sub M2l.pp m.right
    | _ -> .
  and access: waccess t -> _ = fun rest sub -> match rest with
    | Annot Access a :: rest ->
      annot rest (fun ppf -> Pp.fp ppf
                     "@[Annot@ access:%t ... {%a}@]"
                     sub
                     M2l.pp_values a.values
                 )
    | Mt With_access m :: rest ->
      mt rest (fun ppf -> Pp.fp ppf "%a access %t without {%a}"
                  M2l.pp_mt m.body sub
                  (Pp.list Paths.S.pp) (Paths.S.Set.elements m.deletions)
              )
    | _ -> .
  and path_expr: Paths.Expr.t t -> _ = fun rest sub -> match rest with
    | Mt Ident :: rest -> mt rest sub
    | Path_expr Arg _a :: rest -> (* TODO *)
      path_expr rest sub
    | _ -> .
  and ext: M2l.extension_core t -> _ = fun rest sub -> match rest with
    | Expr Extension_node name :: rest ->
      expr rest (fun ppf -> Pp.fp ppf "[%%%s %t]" name sub)
    | Me Extension_node name :: rest ->
      me rest (fun ppf -> Pp.fp ppf "[%%%s %t]" name sub)
    | Mt Extension_node name :: rest ->
      mt rest (fun ppf -> Pp.fp ppf "[%%%s %t]" name sub)
    | _ -> .
  let pp ppf z = zipper z ppf
end
