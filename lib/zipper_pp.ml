
type dp = Format.formatter -> unit
type 'a dprinter = 'a -> dp


module type Result_printer = sig
  module T: Zipper_def.tree

  val pp_opens: T.opens -> dp -> dp
  val pp_bindrec: T.bind_rec dprinter
  val pp_me: T.module_expr dprinter
  val pp_mt: T.module_type dprinter
  val pp_m2l: T.m2l dprinter
  val pp_packed: T.packed dprinter
  val pp_values: T.values dprinter
  val pp_access: T.access dprinter
  val pp_path: T.path dprinter
  val pp_path_expr_args: T.path_expr_args dprinter

end


module Make(Def:Zipper_def.s)(R:Result_printer with module T := Def.T) = struct
  open Def
  module Sk = Zipper_skeleton
  let fp1 fmt x ppf =Pp.fp ppf fmt x
  let fp2 fmt x y ppf =Pp.fp ppf fmt x y
  let optname ppf = function
    | None -> Format.fprintf ppf "_"
    | Some s -> Format.pp_print_string ppf s
  let r_arg x ppf = match x with
    | None -> ()
    | Some (l,_) -> Pp.fp ppf "%a:%t" Name.pp_opt l.Module.Arg.name (R.pp_mt l.signature.Zipper_def.user)
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
    | Me (Open_me_left {left;right;expr; diff=_}) :: rest ->
      me rest (R.pp_opens left (fun ppf ->
          Pp.fp ppf "%t.(%t.(%a))" x (dlist path right) M2l.pp_me expr
        )
        )
    | _ -> .
  and me: M2l.module_expr t -> _ = fun rest sub ->
    match rest with
    | Me (Apply_left right) :: rest ->
      me rest (fun ppf -> Pp.fp ppf "%t(%a)" sub M2l.pp_me right)
    | Me Apply_right left :: rest -> me rest (fp2 "%t(%t)" (R.pp_me left.user) sub)
    | Me Fun_right left :: rest -> me rest (fp2 "functor (%t) ->%t" (r_arg left) sub)
    | Me Constraint_left mt :: rest ->
      me rest (fun ppf -> Pp.fp ppf "(%t:%a)" sub M2l.pp_mt mt)
    | Me Open_me_right {opens; _} :: rest -> me rest (R.pp_opens opens sub)
    | Expr Include :: rest -> expr rest (fp1 "include %t" sub)
    | Expr Bind name :: rest ->
      expr rest (fun ppf -> Pp.fp ppf "module %a=%t" optname name sub)
    | Expr Bind_rec m :: rest ->
      let pp ppf =
        Pp.fp ppf "%t %a: %t =%t@,%a" (R.pp_bindrec m.left.user) Name.pp_opt m.name
          (R.pp_mt m.mt) sub
          (Pp.list (fun ppf (name,_,me) ->
               Pp.fp ppf "@,and %a:?=%a" Name.pp_opt name M2l.pp_me me)
          ) m.right in
      expr rest pp
    | Expr Open :: rest -> expr rest (fp1 "open %t" sub)
    | Annot Packed p :: rest ->
      annot rest (fun ppf -> Pp.fp ppf
                     "@[Annot@ packed:%t%t ... {%a%a%a}@]"
                     (R.pp_packed p.left)
                     sub M2l.pp_packed p.right
                     M2l.pp_access p.access
                     M2l.pp_values p.values
                 )
    | Mt Of :: rest -> mt rest (fp1 "module type of %t" sub)
    | _ -> .
  and annot: M2l.annotation t -> _ = fun rest sub -> match rest with
    | Expr Minor :: rest -> expr rest sub
    | Me Val :: rest -> me rest (fp1 "(val %t)" sub)
    | Ext Val :: rest -> ext rest sub
    | _ -> .
  and mt: M2l.module_type t -> _ = fun rest sub -> match rest with
    | Expr Bind_sig name :: rest ->
      expr rest (fun ppf -> Pp.fp ppf "module type %a=%t" optname name sub)
    | Me Fun_left {name;body} :: rest ->
      me rest
        (fun ppf -> Pp.fp ppf "fun(%a:%t)->%a" Name.pp_opt name sub M2l.pp_me body)
    | Mt Fun_left {name;body} :: rest ->
      mt rest
        (fun ppf -> Pp.fp ppf "fun(%a:%t)->%a" Name.pp_opt name sub M2l.pp_mt body)
    | Mt Fun_right left :: rest ->
      mt rest (fp2 "functor(%t)->%t" (r_arg left) sub)
    | Expr SigInclude :: rest ->
      expr rest (fun ppf -> Pp.fp ppf "include %t" sub)
    | Expr Bind_rec_sig m :: rest ->
      expr rest (fun ppf ->
          Pp.fp ppf "module rec %a %a:%t=%a@,%a"
            (Pp.list ~sep:(Pp.const "@ and ") (fun ppf (name,mt,me) ->
                 Pp.fp ppf "%a:%t=%a" optname name  (R.pp_mt mt) M2l.pp_me me)
            ) m.left
            optname m.name sub M2l.pp_me m.expr
            (Pp.list (fun ppf {M2l.name;expr} ->
                 Pp.fp ppf "and@ %a = %a" optname name M2l.pp_me expr)
            ) m.right
        )
    | Me Constraint_right left :: rest -> me rest (fp2 "(%t:%t)" (R.pp_me left.user) sub)
    | Mt With_body m :: rest ->
      mt rest (fun ppf -> Pp.fp ppf "%t without {%a}" sub
                  (Pp.list Paths.S.pp) (Paths.S.Set.elements m.deletions)
              )
    | _ -> .
  and expr: M2l.expression t -> _ = fun rest sub -> match rest with
    | M2l m :: rest ->
      m2l rest (
        fun ppf -> Pp.fp ppf "%t@ %t@ %a" (R.pp_m2l m.left.user) sub M2l.pp m.right
      )
   | _ -> .
  and access: waccess t -> _ = fun rest sub -> match rest with
    | Annot Access a :: rest ->
      annot rest (fun ppf -> Pp.fp ppf
                     "@[Annot@ access:{%t;%t ... %a}@]"
                     (R.pp_packed a.packed)
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
    | Path_expr Arg a :: rest -> (* TODO *)
      path_expr rest
        (fun ppf -> Pp.fp ppf "@[%t[%t;@ %d,@ %t;@ %a]@]"
            (R.pp_path a.main.user) (R.pp_path_expr_args a.left) a.pos sub
            (Pp.list ~sep:(Pp.const ";@ ") (fun ppf (n,d) -> Pp.fp ppf "%d,@ %a" n Paths.E.pp d))
            a.right
        )
    | _ -> .
  and ext: M2l.extension_core t -> _ = fun rest sub -> match rest with
    | Expr Extension_node name :: rest ->
      expr rest (fun ppf -> Pp.fp ppf "[%%%s %t]" name sub)
    | Me Extension_node name :: rest ->
      me rest (fun ppf -> Pp.fp ppf "[%%%s %t]" name sub)
    | Mt Extension_node name :: rest ->
      mt rest (fun ppf -> Pp.fp ppf "[%%%s %t]" name sub)
    | _ -> .
  and m2l: M2l.t t -> _ = fun rest sub -> match rest with
    | Annot Values a :: rest ->
      annot rest
        (fun ppf -> Pp.fp ppf "annot {%t;%t;%t,%t,%a}"
            (R.pp_packed a.packed)
            (R.pp_access a.access)
            (R.pp_values a.left)
            sub
            M2l.pp_values a.right
        )
    | Me Str :: rest -> me rest (fp1 "struct %t end" sub)
    | Mt Sig :: rest -> mt rest (fp1 "sig %t end" sub)
    | Ext Mod :: rest -> ext rest sub
    | [] -> sub
    | _ -> .
  let pp ppf z = zipper z ppf
end


module Opaque(X:Zipper_def.s) : Result_printer with module T := X.T = struct
  let const fmt _ ppf = Pp.fp ppf fmt
  let pp_m2l = const "..."
  let pp_me = const "..."
  let pp_mt = const "..."
  let pp_values = const "..."
  let pp_packed = const "..."
  let pp_access = const "..."
  let pp_path = const "..."
  let pp_opens _ sub = sub
  let pp_path_expr_args = const "..."
  let pp_bindrec = const "..."
end
