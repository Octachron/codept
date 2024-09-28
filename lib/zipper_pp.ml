
type dp = Format.formatter -> unit
type 'a dprinter = 'a -> dp


module type Result_printer = sig
  module T: Zipper_def.tree

  val pp_opens: T.opens -> dp -> dp
  val pp_bindrec: T.bind_rec dprinter
  val pp_me: T.module_expr dprinter
  val pp_mt: T.module_type dprinter
  val pp_with_constraints: T.with_constraints dprinter
  val pp_m2l: T.m2l dprinter
  val pp_minor: T.minor dprinter
  val pp_minors: T.minors dprinter
  val pp_access: T.access dprinter
  val pp_path: T.path dprinter
  val pp_path_expr: T.path_expr dprinter

end

module Make(Def:Zipper_def.s)(R:Result_printer with module T := Def.T) = struct
  open Def
  module Sk = Zipper_skeleton
  let option transition main x ppf = match x with
    | None -> ()
    | Some x -> Pp.fp ppf "%s%a" transition main x
  let const f x ppf = f ppf x
  let fp1 fmt x ppf =Pp.fp ppf fmt x
  let fp2 fmt x y ppf =Pp.fp ppf fmt x y
  let fp3 fmt x y z ppf =Pp.fp ppf fmt x y z
  let fp4 fmt x y z w ppf =Pp.fp ppf fmt x y z w
  let optname ppf = function
    | None -> Format.fprintf ppf "_"
    | Some s -> Format.pp_print_string ppf s
  let r_arg x ppf = match x with
    | None -> ()
    | Some (l,_) -> Pp.fp ppf "%a:%t" Name.pp_opt l.Module.Arg.name (R.pp_mt l.signature.Zipper_def.user)
  let pp_delete d ppf = Pp.fp ppf (if d then  ":=" else "=")
  let path_loc p ppf = Paths.S.pp ppf p.Loc.data
  let path p ppf = Paths.S.pp ppf p
  let leaf p ppf = Format.fprintf ppf "%t?" (path p.Zipper_skeleton.path)
  let dlist elt l ppf = Pp.list (fun ppf x -> elt x ppf) ppf l
  let a ppf (x,_) = Paths.E.pp ppf x
  let rec zipper (z: Sk.path_in_context zipper)  =
    let f = z.focus in
    let x = leaf f in
    match z.path with
    | Me Ident :: rest -> me (rest:M2l.module_expr t) x
    | Mt Alias :: rest -> mt (rest:M2l.module_type t) x
    | Path_expr Simple :: rest -> path_expr (rest:Paths.Expr.t t) x
    | Me (Open_me_left {left;right;expr; loc=_; diff=_}) :: rest ->
      me (rest: M2l.module_expr t) (R.pp_opens left (fun ppf ->
          Pp.fp ppf "%t.(%t.(%a))" x (dlist path_loc right) M2l.pp_me expr
        )
        )
    | Path_expr Proj (_app,_proj) :: rest ->
      path_expr (rest:Paths.Expr.t t) x
    | With_constraint With_module {body;lhs; delete} :: rest ->
      with_constraint (rest: M2l.with_constraint t)
        (fp4 "%t with module %t %t %t"
           (R.pp_with_constraints body.user)
           (const Paths.S.pp lhs)
           (pp_delete delete)
           x
        )
    | With_constraint With_lhs {body;delete; lhs=_; rhs=Type t} :: rest ->
      with_constraint (rest: M2l.with_constraint t)
        (fp4 "%t with type %t%t%t"
           (R.pp_with_constraints body.user)
           x
           (pp_delete delete)
           (const M2l.pp_annot t)
        )

    | With_constraint With_lhs {body;delete; lhs=_; rhs=Module p} :: rest ->
      with_constraint (rest: M2l.with_constraint t)
        (fp4 "%t with module %t%t%t"
           (R.pp_with_constraints body.user)
           x
           (pp_delete delete)
           (const Paths.S.pp p.data)
        )
    | With_constraint With_lhs {body;delete; lhs=_; rhs=Module_type t} :: rest ->
      with_constraint (rest: M2l.with_constraint t)
        (fp4 "%t with module type %t%t%t"
           (R.pp_with_constraints body.user)
           x
           (pp_delete delete)
           (const M2l.pp_mt t)
        )
    | _ -> .
  and me: M2l.module_expr t -> _ = fun rest sub ->
    match rest with
    | Me (Apply_left right) :: rest ->
      me rest (fun ppf -> Pp.fp ppf "%t(%a)" sub M2l.pp_me right)
    | Me Apply_right left :: rest -> me rest (fp2 "%t(%t)" (R.pp_me left.user) sub)
    | Me (Proj_left right) :: rest ->
      me rest (fun ppf -> Pp.fp ppf "%t.%a" sub Paths.Simple.pp right)
    | Me Fun_right left :: rest -> me rest (fp2 "functor (%t) ->%t" (r_arg left) sub)
    | Me Constraint_left mt :: rest ->
      me rest (fun ppf -> Pp.fp ppf "(%t:%a)" sub M2l.pp_mt mt)
    | Me Open_me_right {opens; _} :: rest -> me rest (R.pp_opens opens sub)
    | Expr Include :: rest -> expr (rest: M2l.expression t) (fp1 "include %t" sub)
    | Expr Bind name :: rest ->
      expr (rest: M2l.expression t) (fun ppf -> Pp.fp ppf "module %a=%t" optname name sub)
    | Expr Bind_rec m :: rest ->
      let pp ppf =
        Pp.fp ppf "%t %a: %t =%t@,%a" (R.pp_bindrec m.left.user) Name.pp_opt m.name
          (R.pp_mt m.mt) sub
          (Pp.list (fun ppf (name,_,me) ->
               Pp.fp ppf "@,and %a:?=%a" Name.pp_opt name M2l.pp_me me)
          ) m.right in
      expr (rest:M2l.expression t) pp
    | Expr Open :: rest -> expr (rest:M2l.expression t) (fp1 "open %t" sub)
    | Mt Of :: rest -> mt rest (fp1 "module type of %t" sub)
    | Minor Pack :: rest ->
      minor (rest:M2l.minor t) (fp1 "(module %t)" sub)
    | Minor Local_bind_left (_diff, name,right) :: rest ->
      minor (rest:M2l.minor t) (fun ppf -> Pp.fp ppf "%a=%t in %a"
                     Name.pp_opt name
                     sub
                     M2l.pp_annot right
                 )
    | Minor Local_open_left (_diff,_,minors) :: rest ->
        minor (rest:M2l.minor t) (fun ppf -> Pp.fp ppf "open %t in %a"
                       sub
                       M2l.pp_annot minors
                   )
    | _ -> .
  and minor: M2l.minor t -> _ = fun rest sub -> match rest with
    | Minors {left; right} :: rest ->
      minors (rest:M2l.minor list t) (fun ppf ->
          Pp.fp ppf "%t%t%a" (R.pp_minors left)
            sub
            M2l.pp_annot right
        )
  and minors: M2l.minor list t -> _ = fun rest sub -> match rest with
    | Expr Minors :: rest ->
      expr rest sub
    | Minor (Local_open_right (_diff,e)) :: rest ->
      minor rest (fun ppf ->
          Pp.fp ppf "open %t in %t" (R.pp_me e.user) sub
        )
    | Minor (Local_bind_right (_diff,name,expr)) :: rest ->
      minor rest (fun ppf ->
          Pp.fp ppf "%a=%t in %t" optname name (R.pp_me expr.user) sub
        )
    | Me Val :: rest -> me rest (fp1 "(val %t)" sub)
    | Ext Val :: rest -> ext (rest:M2l.extension_core t) sub
    | With_constraint With_type body :: rest ->
      with_constraint rest (fp2 "%t with type %t" (R.pp_with_constraints body.user) sub)
    | _ -> .

  and mt: M2l.module_type t -> _ = fun rest sub -> match rest with
    | Expr Bind_sig name :: rest ->
      expr rest (fun ppf -> Pp.fp ppf "module type %a=%t" optname name sub)
    | Me Fun_left {name;diff=_;body} :: rest ->
      me rest
        (fun ppf -> Pp.fp ppf "fun(%a:%t)->%a" Name.pp_opt name sub M2l.pp_me body)
    | Mt Fun_left {name;diff=_;body} :: rest ->
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
    | With_constraint With_module_type {body;lhs;delete} :: rest ->
      with_constraint (rest: M2l.with_constraint t)
        (fp4 "%t with module type %t %t %t"
           (R.pp_with_constraints body.user)
           (const Paths.S.pp lhs)
           (pp_delete delete)
           sub
        )
    | Mt With_body wcstrs :: rest ->
      mt rest (fp2 "%t with %t" sub (const M2l.pp_with_constraints wcstrs))
    | _ -> .
  and with_constraint: M2l.with_constraint t -> _ = fun rest sub ->
    match rest with
    | Mt With_constraints {original_body; right} :: rest ->
      mt rest (fp3 "%t with %t%t"
                 (R.pp_mt original_body.user)
                 sub
                 (const M2l.pp_with_constraints right)
               )
    | _ -> .
  and expr: M2l.expression t -> _ = fun rest sub -> match rest with
    | M2l m :: rest ->
      m2l (rest:M2l.m2l t) (
        fun ppf -> Pp.fp ppf "%t@ %t@ %a" (R.pp_m2l m.left.user) sub M2l.pp m.right
      )
   | _ -> .
  and access: waccess t -> _ = fun rest sub -> match rest with
    | Minor Access :: rest ->
      minor rest (fp1 "access %t" sub)
    | _ -> .
  and path_expr: Paths.Expr.t t -> _ = fun rest sub -> match rest with
    | Mt Ident :: rest -> mt rest sub
    | Path_expr App_f (a,proj) :: rest ->
      path_expr rest
        (fun ppf -> Pp.fp ppf "@[%t(%a)%t@]"
            sub Paths.E.pp a (option "." Paths.S.pp (Option.fmap (fun (_,_,x) -> x) proj))
        )
   | Path_expr App_x (f,proj) :: rest ->
      path_expr rest
        (fun ppf -> Pp.fp ppf "@[%t(%t)%t@]"
            (R.pp_path_expr f.user) sub (option "." Paths.S.pp (Option.fmap (fun (_,_,x) -> x) proj))
        )
   | Access acc :: rest ->
     access (rest: waccess t)
       (fun ppf -> Pp.fp ppf "access %t...%t...%a"
          (R.pp_access acc.left)
          sub
          (Pp.list a) acc.right
       )
   | _ -> .
  and ext: M2l.extension_core t -> _ = fun rest sub -> match rest with
    | Expr Extension_node name :: rest ->
      expr rest (fun ppf -> Pp.fp ppf "[%%%s %t]" name sub)
    | Me Extension_node name :: rest ->
      me rest (fun ppf -> Pp.fp ppf "[%%%s %t]" name sub)
    | Mt Extension_node name :: rest ->
      mt rest (fun ppf -> Pp.fp ppf "[%%%s %t]" name sub)
    | Minor Extension_node name :: rest ->
      minor rest (fun ppf -> Pp.fp ppf "[%%%s %t]" name sub)
    | _ -> .
  and m2l: M2l.t t -> _ = fun rest sub -> match rest with
    | Me Str :: rest -> me rest (fp1 "struct %t end" sub)
    | Mt Sig :: rest -> mt rest (fp1 "sig %t end" sub)
    | Ext Mod :: rest -> ext rest sub
    | [] -> sub
    | _ -> .
  let pp ppf z = zipper z ppf
end


module Opaque(X:Zipper_def.s) : Result_printer with module T := X.T = struct
  let const fmt _ ppf = Pp.fp ppf fmt
  let ellipsis ppf = const "..." ppf
  let pp_m2l = ellipsis
  let pp_me = ellipsis
  let pp_mt = ellipsis
  let pp_access = ellipsis
  let pp_path = ellipsis
  let pp_with_constraints = ellipsis
  let pp_opens _ sub = sub
  let pp_path_expr = ellipsis
  let pp_bindrec = ellipsis
  let pp_minors = ellipsis
  let pp_minor = ellipsis
end
