
exception Include_functor

module L = Longident
module B = M2l.Build

module D = Definition
module S = Module.Sig

let rec from_lid  =
  let open Epath in
  function
    | L.Lident s -> A s
    | L.Ldot (lid,s) -> S(from_lid lid,s)
    | L.Lapply (f,_x) -> F (from_lid f) (* argument are forgotten *)

let module_path =
  function
    | L.Lident _ -> None
    | L.Ldot (lid, _) -> Some (from_lid lid)
    | L.Lapply _ -> None

let value = Epath.Module
let type_ = Epath.Module_type

open Parsetree
let txt x= x.Location.txt

let epath x = from_lid @@ txt x
let npath x = Epath.concrete @@ epath x

let access lid =
  let open Epath in
  match from_lid @@ txt lid with
  | A _ -> Name.Set.empty
  | S(p,_) -> Name.Set.singleton @@ prefix p
  | T | F _ -> assert false

let access' lid =
  let open Epath in
 match from_lid @@ txt lid with
  | A _ -> []
  | S(p,_) -> [ B.access p ]
  | T | F _ -> assert false


let do_open lid =
  [M2l.Open (npath lid)]

let (+?) x l = match x with None -> l | Some x -> x :: l

(*
let do_include kind extract env m=
  let m' = extract env m in
  R.include_ kind env m'
*)

let first_class_approx = S.empty

let opt f x=  Option.( x >>| f >< [] )
let flip f x y = f y x

let (@%) l l' =
  let open M2l in
  match l,l' with
  | [Minor m] , Minor m' :: q -> Minor(merge_annot m m') :: q
  | _ -> l @ l'

let rec gen_mmap (@) f = function
  | [] -> []
  | a :: q -> (f a) @ gen_mmap (@) f q

let mmap f = gen_mmap (@%) f
let gmmap f = gen_mmap (@) f


let (+:) s l' =
  if Name.Set.cardinal s = 0 then l'
  else
    let open M2l in
    match l' with
    | Minor m :: q ->
      Minor { m with access = Name.Set.union s m.access} :: q
    | _ -> Minor { empty_annot with access = s } ::  l'


open M2l
let rec structure str =
  mmap structure_item str
and structure_item item =
  match item.pstr_desc with
  | Pstr_eval (exp, _attrs) -> expr exp
  (* ;; exp [@@_attrs ] *)
  | Pstr_value (_rec_flag, vals)
    (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
         *) ->
    mmap value_binding vals
  | Pstr_primitive desc
        (*  val x: T
            external x: T = "s1" ... "sn" *)
    -> core_type desc.pval_type
  | Pstr_type (_rec_flag, type_declarations)
    (* type t1 = ... and ... and tn = ... *) ->
    mmap type_declaration type_declarations
  | Pstr_typext a_type_extension  (* type t1 += ... *) ->
    type_extension a_type_extension
  | Pstr_exception an_extension_constructor
        (* exception C of T
           exception C = M.X *)
    -> extension_constructor an_extension_constructor
  | Pstr_module mb (* module X = ME *) ->
    [Bind( Module, module_binding_raw mb)]
  | Pstr_recmodule module_bindings (* module rec X1 = ME1 and ... and Xn = MEn *)
    -> recmodules module_bindings
  | Pstr_modtype a_module_type_declaration (*module type s = .. *) ->
    [ Bind(Module_type, module_type_declaration a_module_type_declaration) ]
  | Pstr_open open_desc (* open M *) ->
        do_open  open_desc.popen_lid
  | Pstr_class class_declarations  (* class c1 = ... and ... and cn = ... *)
    -> mmap class_declaration class_declarations
  | Pstr_class_type class_type_declarations
  (* class type ct1 = ... and ... and ctn = ... *)
    -> mmap class_type_declaration class_type_declarations
  | Pstr_include include_dec (* include M *) ->
        do_include include_dec
  | Pstr_attribute _attribute (* [@@@id] *)
    -> []
  | Pstr_extension (_id, _payload) (* [%%id] *) ->
    Warning.extension(); []
and expr exp =
  match exp.pexp_desc with
  | Pexp_ident name (* x, M.x *) ->
    access' name
  | Pexp_let (_rec_flag, value_bindings, exp )
        (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
        *)
    ->
    mmap value_binding value_bindings @% expr exp (** todo: module bindings *)
  | Pexp_function cases (* function P1 -> E1 | ... | Pn -> En *) ->
    mmap case cases
  | Pexp_fun ( _arg_label, expr_opt, pat, expression)
        (* fun P -> E1                          (Simple, None)
           fun ~l:P -> E1                       (Labelled l, None)
           fun ?l:P -> E1                       (Optional l, None)
           fun ?l:(P = E0) -> E1                (Optional l, Some E0)
           Notes:
           - If E0 is provided, only Optional is allowed.
           - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
           - "let f P = E" is represented using Pexp_fun.
        *)
    ->
    (opt expr expr_opt)
    @% pattern pat
    @% expr expression
  | Pexp_apply (expression, args)
        (* E0 ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           Invariant: n > 0
        *)
    ->
    expr expression @%
    mmap (fun (_,e) -> expr e) args
  | Pexp_match (expression, cases)
    (* match E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_try (expression, cases)
    (* try E0 with P1 -> E1 | ... | Pn -> En *)
    ->
    expr expression @% mmap case cases
  | Pexp_tuple expressions
      (* (E1, ..., En) Invariant: n >= 2 *)
   ->
      mmap expr expressions
  | Pexp_construct (constr, expr_opt)
        (* C                None
           C E              Some E
           C (E1, ..., En)  Some (Pexp_tuple[E1;...;En])
        *) ->
    access constr +: opt expr  expr_opt
  | Pexp_variant (_label, expression_opt)
        (* `A             (None)
           `A E           (Some E)
        *)
    -> opt expr expression_opt
  | Pexp_record (labels, expression_opt)
        (* { l1=P1; ...; ln=Pn }     (None)
           { E0 with l1=P1; ...; ln=Pn }   (Some E0)

           Invariant: n > 0
        *)
    ->
    opt expr expression_opt
    @% mmap (fun (labl,expression) -> access labl +:  expr expression ) labels
  | Pexp_field (expression, field)  (* E.l *) ->
    access field +: expr expression
  | Pexp_setfield (e1, field,e2) (* E1.l <- E2 *) ->
    access field +: (expr e1 @% expr e2)
  | Pexp_array expressions (* [| E1; ...; En |] *) ->
    mmap expr expressions
  | Pexp_ifthenelse (e1, e2, e3) (* if E1 then E2 else E3 *) ->
    expr e1 @% expr e2 @% opt expr e3
  | Pexp_sequence (e1,e2) (* E1; E2 *) ->
    expr e1 @% expr e2
  | Pexp_while (e1, e2) (* while E1 do E2 done *) ->
    expr e1 @% expr e2
  | Pexp_for (pat, e1, e2,_,e3)
  (* for pat = E1 to E2 do E3 done      (flag = Upto)
     for pat = E1 downto E2 do E3 done  (flag = Downto)
  *) ->
    pattern pat @% expr e1 @% expr e2 @% expr e3
  | Pexp_constraint (e,t) (* (E : T) *) ->
    expr e @% core_type t
  | Pexp_coerce (e, t_opt, coer)
  (* (E :> T)        (None, T)
     (E : T0 :> T)   (Some T0, T)
  *) ->
    expr e @% opt core_type t_opt @% core_type coer
  | Pexp_new name (* new M.c *) ->
    access' name
  | Pexp_setinstvar (_x, e) (* x <- e *) ->
    expr e
  | Pexp_override labels (* {< x1 = E1; ...; Xn = En >} *) ->
    mmap (fun (_,e) -> expr e) labels
  | Pexp_letmodule (m, me, e) (* let module M = ME in E *) ->
    [ B.value ( Bind( Module, module_binding (m,me) ) :: expr e ) ]
(*  | Pexp_letexception (c, e) (* let exception C in E *) ->
    expression (extension_constructor env ext) e *)
  | Pexp_send (e, _) (*  E # m *)
  | Pexp_assert e (* assert E *)
  | Pexp_newtype (_ ,e) (* fun (type t) -> E *)
  | Pexp_lazy e (* lazy E *) -> expr  e

  | Pexp_poly (e, ct_opt) ->
    expr e @% opt core_type ct_opt
  | Pexp_object clstr (* object ... end *) ->
    class_structure clstr
  | Pexp_pack me (* (module ME) *)
    -> Warning.first_class_module ();
       (* todo: are all cases caught by the Module.approximation mechanism?  *)
    [ B.opaque (module_expr me) ]
  | Pexp_open (_override_flag,name,e)
        (* M.(E), let open M in E, let! open M in E *)
    -> [ B.value ( do_open name @% expr e ) ]
  | Pexp_extension (name, PStr payload) when txt name = "extension_constructor" ->
    structure payload
  | Pexp_constant _ | Pexp_unreachable (* . *)
    -> []
  | Pexp_extension _ (* [%ext] *) -> (Warning.extension(); [])
and pattern pat = match pat.ppat_desc with
  | Ppat_constant _ (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Ppat_interval _ (* 'a'..'z'*)
  | Ppat_any
  | Ppat_extension _
  | Ppat_var _ (* x *) -> []

  | Ppat_exception pat (* exception P *)
  | Ppat_lazy pat (* lazy P *)
  | Ppat_alias (pat,_) (* P as 'a *) -> pattern pat

  | Ppat_array patterns (* [| P1; ...; Pn |] *)
  | Ppat_tuple patterns (* (P1, ..., Pn) *) ->
    mmap pattern patterns

  | Ppat_construct (c, p)
        (* C                None
           C P              Some P
           C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
        *) ->
    access c +: opt pattern p
  | Ppat_variant (_, p) (*`A (None), `A P(Some P)*) ->
    opt pattern p
  | Ppat_record (fields, _flag)
        (* { l1=P1; ...; ln=Pn }     (flag = Closed)
           { l1=P1; ...; ln=Pn; _}   (flag = Open)
        *) ->
    mmap (fun (_,p) -> pattern p ) fields
  | Ppat_or (p1,p2) (* P1 | P2 *) ->
    pattern p1 @% pattern p2
  | Ppat_constraint(
      {ppat_desc=Ppat_unpack name; _},
      {ptyp_desc=Ptyp_package s; _ } ) ->
    let _name = txt name in
    let _s, others = full_package_type s in
    others
    (* todo : catch higher up *)
  | Ppat_constraint (pat, ct)  (* (P : T) *) ->
    pattern pat @% core_type ct
  | Ppat_type name (* #tconst *) -> access' name
  | Ppat_unpack m ->
    (* Warning.first_class_module(); todo: test coverage *)
    [ B.value [ Defs (Def.md
                      { M.name = txt m ;
                        args = [];
                        origin = First_class;
                        signature = first_class_approx
                      })
            ]
    ]
      (* (module P)
           Note: (module P : S) is represented as
           Ppat_constraint(Ppat_unpack, Ptyp_package)
         *)
(*  | Ppat_open (m,p) (* M.(P) *) ->
    Resolver.(up env.Envt.signature) @@ pattern (do_open env m) p *)

and type_declaration td  =
  mmap (fun (_,t,_) -> core_type t) td.ptype_cstrs
  @% type_kind td.ptype_kind
  @% opt core_type td.ptype_manifest
and type_kind = function
  | Ptype_abstract | Ptype_open -> []
  | Ptype_variant constructor_declarations ->
      mmap constructor_declaration constructor_declarations
  | Ptype_record label_declarations ->
    mmap label_declaration label_declarations
and constructor_declaration cd =
  opt core_type cd.pcd_res @% constructor_args cd.pcd_args
and constructor_args = function
    | Pcstr_tuple cts -> mmap core_type cts
    | Pcstr_record lds -> mmap label_declaration lds
and label_declaration ld = core_type ld.pld_type
and type_extension tyext =
  access tyext.ptyext_path
  +: mmap  extension_constructor tyext.ptyext_constructors
and core_type ct = match ct.ptyp_desc with
  | Ptyp_any  (*  _ *)
  | Ptyp_extension _ (* [%id] *)
  | Ptyp_var _ (* 'a *) -> []
  | Ptyp_arrow (_, t1, t2) (* [~? ]T1->T2 *) ->
      core_type t1 @% core_type t2
  | Ptyp_tuple cts (* T1 * ... * Tn *) ->
    mmap core_type cts
  | Ptyp_class (name,cts)
  | Ptyp_constr (name,cts) (*[|T|(T1n ..., Tn)] tconstr *) ->
    access name +: mmap core_type  cts
  | Ptyp_object (lbls, _ ) (* < l1:T1; ...; ln:Tn[; ..] > *) ->
    mmap (fun  (_,_,t) -> core_type t) lbls
  | Ptyp_poly (_, ct)
  | Ptyp_alias (ct,_) (* T as 'a *) -> core_type ct

  | Ptyp_variant (row_fields,_,_labels) ->
    mmap row_field row_fields
  | Ptyp_package s (* (module S) *) ->
    package_type s

and row_field = function
  | Rtag (_,_,_,cts) -> mmap core_type cts
  | Rinherit ct -> core_type ct
and package_type (s,constraints) =
  access s +:
  mmap (fun  (_,ct) -> core_type ct) constraints
and full_package_type (s,constraints) =
  Ident (epath s),
  mmap (fun (_,ct) -> core_type ct) constraints
and case cs =
  pattern cs.pc_lhs
  @% opt expr cs.pc_guard
  @% expr cs.pc_rhs
and do_include incl =
    [ Include (module_expr incl.pincl_mod) ]
and extension_constructor extc = match extc.pext_kind with
  | Pext_decl (args, cto) ->
    constructor_args args
    @% opt core_type cto
| Pext_rebind name -> access' name
and class_type ct = match ct.pcty_desc with
  | Pcty_constr (name, cts ) (* c ['a1, ..., 'an] c *) ->
    access name +: mmap core_type cts
  | Pcty_signature cs (* object ... end *) -> class_signature cs
  | Pcty_arrow (_arg_label, ct, clt) (* ^T -> CT *) ->
    class_type clt @% core_type ct
  | Pcty_extension _ (* [%ext] *) -> []
and class_signature cs = mmap class_type_field cs.pcsig_fields
and class_type_field ctf = match ctf.pctf_desc with
  | Pctf_inherit ct -> class_type ct
  | Pctf_val ( _, _, _, ct) (*val x : T *)
  | Pctf_method (_ ,_,_,ct) (* method x: T *)
    -> core_type ct
  | Pctf_constraint  (t1, t2) (* constraint T1 = T2 *) ->
    core_type t2 @% core_type t1
  | Pctf_attribute _ -> []
  | Pctf_extension _ -> Warning.extension (); []
and class_structure ct =
  mmap class_field ct.pcstr_fields
and class_field  field = match field.pcf_desc with
  | Pcf_inherit (_override_flag, ce, _) (* inherit CE *) ->
    class_expr ce
  | Pcf_method (_, _, cfk)
  | Pcf_val (_,_, cfk) (* val x = E *)->
    class_field_kind cfk
  | Pcf_constraint (_ , ct) (* constraint T1 = T2 *) ->
    core_type ct
  | Pcf_initializer e (* initializer E *) -> expr e
  | Pcf_attribute _
  | Pcf_extension _ -> Warning.extension (); []
and class_expr ce = match ce.pcl_desc with
  | Pcl_constr (name, cts)  (* ['a1, ..., 'an] c *) ->
    access name +: mmap core_type cts
  | Pcl_structure cs (* object ... end *) -> class_structure cs
  | Pcl_fun (_arg_label, eo, pat, ce)
        (* fun P -> CE                          (Simple, None)
           fun ~l:P -> CE                       (Labelled l, None)
           fun ?l:P -> CE                       (Optional l, None)
           fun ?l:(P = E0) -> CE                (Optional l, Some E0)
        *)
    -> opt expr eo @% pattern pat @% class_expr ce
  | Pcl_apply (ce, les )
        (* CE ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           Invariant: n > 0
        *) ->
    mmap (fun (_,e) -> expr e) les @% class_expr ce
  | Pcl_let (_, value_bindings, ce ) (* let P1 = E1 and ... and Pn = EN in CE *)
    ->
    class_expr ce
    @% mmap value_binding value_bindings
  | Pcl_constraint (ce, ct) ->
    class_type ct @% class_expr ce
  | Pcl_extension _ext -> Warning.extension () ; []
and class_field_kind = function
  | Cfk_virtual ct -> core_type ct
  | Cfk_concrete (_, e) -> expr e
and class_declaration cd = class_expr cd.pci_expr
and class_type_declaration ctd = class_type ctd.pci_expr
and module_expr mexpr : M2l.module_expr =
  match mexpr.pmod_desc with
  | Pmod_ident name (* A *) ->
    Ident (npath name)
  | Pmod_structure str (* struct ... end *) ->
    Str (structure  str)
  | Pmod_functor (name, sign, mex) ->
    let name = txt name in
    let arg = Option.( sign >>| module_type >>| fun s ->{Arg.name;signature=s} ) in
    Fun { arg; body = module_expr mex }
  | Pmod_apply (f,x)  (* ME1(ME2) *) ->
    Apply {f = module_expr f; x = module_expr x }
  | Pmod_constraint (me,mt) ->
    Constraint(module_expr me, module_type mt)
  | Pmod_unpack { pexp_desc = Pexp_constraint
                      (inner, {ptyp_desc = Ptyp_package s; _}); _ }
    (* (val E : S ) *) ->
    Constraint( Opaque (expr inner), fst @@ full_package_type s)
  | Pmod_unpack e  (* (val E) *) ->
    Opaque (expr e)
  | Pmod_extension _extension ->
    Warning.extension();
     Opaque []
        (* [%id] *)
and value_binding vb =
  pattern vb.pvb_pat
  @% expr vb.pvb_expr
and module_binding_raw mb =
  module_binding (mb.pmb_name, mb.pmb_expr)
and module_binding (pmb_name, pmb_expr) =
  { name = txt pmb_name; expr = module_expr pmb_expr }
and module_type (mt:Parsetree.module_type) =
  match mt.pmty_desc with
  | Pmty_signature s (* sig ... end *) -> Sig (signature s)
  | Pmty_functor (name, arg, res) (* functor(X : MT1) -> MT2 *) ->
    let arg = let open Option in
      arg >>| module_type >>| fun s -> { Arg.name = txt name; signature = s} in
    Fun { arg; body = module_type res }
  | Pmty_with (mt, wlist) (* MT with ... *) ->
    let deletions = Name.Set.of_list @@  gmmap dels wlist in
    With { body = module_type mt; deletions }
  | Pmty_typeof me (* module type of ME *) ->
    Of (module_expr me)
  | Pmty_extension _ (* [%id] *) ->
    Warning.extension();
    Opaque
  | Pmty_alias lid -> Warning.confused "Pmty_alias" ;
    Ident (epath lid)
  | Pmty_ident lid (* S *) ->
    Ident (epath lid)
and module_declaration mdec =
  let s = module_type mdec.pmd_type in
  { name = txt mdec.pmd_name; expr = Constraint(Opaque [], s) }
and module_type_declaration mdec =
  let open Option in
  let name = txt mdec.pmtd_name in
  let s = ( (mdec.pmtd_type >>| module_type) >< Sig [] ) in
  {name; expr = Constraint(Opaque [], s) }
and signature sign =
  mmap signature_item sign
and signature_item item =  match item.psig_desc with
  | Psig_value vd (* val x: T *) ->
    core_type vd.pval_type
  | Psig_type (_rec_flag, tds) (* type t1 = ... and ... and tn = ... *) ->
    mmap type_declaration tds
  | Psig_typext te (* type t1 += ... *) ->
    type_extension te
  | Psig_exception ec (* exception C of T *) ->
    extension_constructor ec
  | Psig_module md (* module X : MT *) ->
    [Bind(Module, module_declaration md)]
  | Psig_recmodule mds (* module rec X1 : MT1 and ... and Xn : MTn *) ->
    [Bind_rec (List.map module_declaration mds)]
    (* Warning.confused "Psig_recmodule"; (* todo coverage*) *)
  | Psig_modtype mtd (* module type S = MT *) ->
    [Bind(Module_type, module_type_declaration mtd)]
  | Psig_open od (* open X *) ->
    do_open od.popen_lid
  | Psig_include id (* include MT *) ->
    [ SigInclude (module_type id.pincl_mod) ]
  | Psig_class cds (* class c1 : ... and ... and cn : ... *) ->
    mmap class_description cds
  | Psig_class_type ctds ->
    mmap class_type_declaration ctds
  | Psig_attribute _ -> []
  | Psig_extension _ -> Warning.extension(); []
and class_description x = class_type_declaration x
and recmodules mbs =
  [Bind_rec (List.map module_binding_raw mbs)]
and dels  =
  function
  | Pwith_typesubst _ (* with type t := ... *)
  | Pwith_type _(* with type X.t = ... *) -> []
  | Pwith_module _ (* with module X.Y = Z *) -> []
  | Pwith_modsubst (name, _) ->
    let name = txt name in
    [name]
