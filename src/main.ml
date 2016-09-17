
let test = {|
 open I
 module U = struct module I = struct module E = Ext  end end
 open A
 module K = struct open W.S module X = Y end
 let x = B.x
 module F(X:sig end) = struct module N = Xeno end
 open R
 open C
 open U.I
 open E
 open K.X

|}

module Warning = struct

  let p fmt = Format.eprintf fmt
  let fp fmt = Format.fprintf fmt


  let log pp = Format.eprintf "@[<hov2>\\e[35mWarning\\e[39m:@,@@[%a@]@]@." pp

  let string ppf = fp ppf "%s"
  let log_s s = log string s
  let extension ()= log_s "extension node ignored"
  let confused () = log_s "confused author did not know"

end

module Option = struct
  let fmap f = function
    | Some x -> Some (f x)
    | None -> None

  let default value = function
    | None -> value
    | Some x -> x

  let (>>|) x f= fmap f x
  let (><) opt def = default def opt

end

let lex_test = Lexing.from_channel @@ open_in Sys.argv.(1)

let ast = Parse.implementation lex_test

exception Include_functor

module L = Longident

let rec from_lid  =
  let open Resolver.Path in
  function
    | L.Lident s -> A s
    | L.Ldot (lid,s) -> S(from_lid lid,s)
    | L.Lapply (f,_x) -> F (from_lid f) (* argument are forgotten *)

let module_path =
  function
    | L.Lident _ -> None
    | L.Ldot (lid, _) -> Some (from_lid lid)
    | L.Lapply _ -> None

let value = Resolver.Path.Module
let type_ = Resolver.Path.Module_type


module Env = Resolver.Env

open Parsetree
let txt x= x.Location.txt

let find_signature kind env lid =
  let open Resolver in
  let unresolved = Env.unresolved env in
  let path = from_lid @@ txt lid in
  match Env.find (kind,path) env with
    | Some(Either.Left sign ) -> unresolved, sign
    | Some(Either.Right unk) -> Unresolved.(add_new (Extern unk) unresolved),
                                Module.Alias unk
    | None ->
      Unresolved.(add_new (Env.loc value path) unresolved),
      Module.Alias (Unresolved.alias_with_context env.Env.unresolved (value,path))

let access_gen kind env path = let open Option in
     path |> txt |> module_path >>| (fun p -> Resolver.access (kind,p) env)
  >< env

let access = access_gen value

let do_open_gen kind env popen_lid =
  let path = from_lid @@ txt popen_lid in
  Resolver.open_ kind path env

let do_open = do_open_gen value

let do_include_gen kind extract env m=
  let open Resolver in
  let unresolved, sign = extract env m in
  let merge s s' = Resolver.Module.M.union (fun _k _x y -> Some y) s s' in
  let sign0 = env.Env.signature in
  match sign with
  | Module.Sig {Module.s;includes} ->
    let env = Env.update kind (merge s) env in
      { env with
        Env.signature = Module.{ s = merge sign0.s s;
                      includes = S.union includes sign0.includes
                    };
        unresolved
      }
  | Module.Alias u ->
    let signature = Module.{ sign0 with includes = S.add u sign0.includes } in
    { env with Env.signature; unresolved }
  | Module.Fun _ -> raise Include_functor


let opt f env x=  Option.( x >>| (fun x -> f env x) >< env )
let flip f x y = f y x

let rec structure env str =
  Env.refocus env @@ List.fold_left structure_item env str
and structure_item env item =
  match item.pstr_desc with
  | Pstr_eval (exp, _attrs) -> expr env exp
  (* ;; exp [@@_attrs ] *)
  | Pstr_value (_rec_flag, vals)
    (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
         *) ->
    List.fold_left value_binding env vals
  | Pstr_primitive _desc -> env
        (*  val x: T
            external x: T = "s1" ... "sn" *)
  | Pstr_type (_rec_flag, type_declarations)
    (* type t1 = ... and ... and tn = ... *) ->
    List.fold_left type_declaration env type_declarations
  | Pstr_typext a_type_extension  (* type t1 += ... *) ->
    type_extension env a_type_extension
  | Pstr_exception an_extension_constructor
        (* exception C of T
           exception C = M.X *)
    -> extension_constructor env an_extension_constructor
  | Pstr_module mb (* module X = ME *) ->
    module_binding env (mb.pmb_name, mb.pmb_expr)
  | Pstr_recmodule module_bindings (* module rec X1 = ME1 and ... and Xn = MEn *)
    -> recmodules env module_bindings
  | Pstr_modtype a_module_type_declaration (*module type s = .. *) ->
        module_type_declaration env a_module_type_declaration
  | Pstr_open open_desc (* open M *) ->
        do_open env open_desc.popen_lid
  | Pstr_class class_declarations  (* class c1 = ... and ... and cn = ... *)
    -> List.fold_left class_declaration env class_declarations
  | Pstr_class_type class_type_declarations
  (* class type ct1 = ... and ... and ctn = ... *)
    -> List.fold_left class_type_declaration env class_type_declarations
  | Pstr_include include_dec (* include M *) ->
        do_include env include_dec
  | Pstr_attribute _attribute (* [@@@id] *)
    -> env
  | Pstr_extension (_id, _payload) (* [%%id] *) ->
    env
and expr env exp =
  match exp.pexp_desc with
  | Pexp_ident name (* x, M.x *) ->
    access env name
  | Pexp_let (_rec_flag, value_bindings, exp )
        (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
        *)
    ->
    let env' = List.fold_left value_binding env value_bindings in
    expr env' exp
  | Pexp_function cases (* function P1 -> E1 | ... | Pn -> En *) ->
    List.fold_left case env cases
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
    let env' = opt expr env expr_opt in
    let env' = pattern env' pat in
    expr env' expression
  | Pexp_apply (expression, args)
        (* E0 ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           Invariant: n > 0
        *)
    ->
    let env = expr env expression in
    List.fold_left (fun env (_flag,e) -> expr env e) env args
  | Pexp_match (expression, cases)
    (* match E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_try (expression, cases)
    (* try E0 with P1 -> E1 | ... | Pn -> En *)
    ->
    List.fold_left case (expr env expression) cases
  | Pexp_tuple expressions
      (* (E1, ..., En) Invariant: n >= 2 *)
   ->
      List.fold_left expr env expressions
  | Pexp_construct (constr, expr_opt)
        (* C                None
           C E              Some E
           C (E1, ..., En)  Some (Pexp_tuple[E1;...;En])
        *) ->
    let env = access env constr in
    opt expr env expr_opt
  | Pexp_variant (_label, expression_opt)
        (* `A             (None)
           `A E           (Some E)
        *)
    -> opt expr env expression_opt
  | Pexp_record (labels, expression_opt)
        (* { l1=P1; ...; ln=Pn }     (None)
           { E0 with l1=P1; ...; ln=Pn }   (Some E0)

           Invariant: n > 0
        *)
    ->
    let env =
      List.fold_left (fun env (labl,expression) ->
          let env = access env labl in
          expr env expression )
        env labels in
    opt expr env expression_opt
  | Pexp_field (expression, field)  (* E.l *) ->
    let env = expr env expression in
    access env field
  | Pexp_setfield (e1, field,e2) (* E1.l <- E2 *) ->
    let env = expr env e1 in
    let env = access env field in
    expr env e2
  | Pexp_array expressions (* [| E1; ...; En |] *) ->
    List.fold_left expr env expressions
  | Pexp_ifthenelse (e1, e2, e3) (* if E1 then E2 else E3 *) ->
    expr env e1 |> flip expr e2 |> flip (opt expr) e3
  | Pexp_sequence (e1,e2) (* E1; E2 *) ->
    expr (expr env e1) e2
  | Pexp_while (e1, e2) (* while E1 do E2 done *) ->
    expr (expr env e1) e2
  | Pexp_for (pat, e1, e2,_,e3)
  (* for pat = E1 to E2 do E3 done      (flag = Upto)
     for pat = E1 downto E2 do E3 done  (flag = Downto)
  *) ->
    expr (expr (expr (pattern env pat) e1) e2) e3
  | Pexp_constraint (e,t) (* (E : T) *) ->
    expr (core_type env t) e
  | Pexp_coerce (e, t_opt, coer)
  (* (E :> T)        (None, T)
     (E : T0 :> T)   (Some T0, T)
  *) ->
    expr (opt core_type (core_type env coer) t_opt) e
  | Pexp_new name (* new M.c *) ->
    access env name
  | Pexp_setinstvar (_x, e) (* x <- e *) ->
    expr env e
  | Pexp_override labels (* {< x1 = E1; ...; Xn = En >} *) ->
    List.fold_left (fun env (_,e) -> expr env e) env labels
  | Pexp_letmodule (m, me, e) (* let module M = ME in E *) ->
    let env' = module_binding env (m,me) in
    let env' = expr env' e in
    Env.{ (refocus env env') with signature = env.signature }
(*  | Pexp_letexception (c, e) (* let exception C in E *) ->
    expression (extension_constructor env ext) e *)

  | Pexp_send (e, _) (*  E # m *)
  | Pexp_assert e (* assert E *)
  | Pexp_newtype (_ ,e) (* fun (type t) -> E *)
  | Pexp_lazy e (* lazy E *) -> expr env e

  | Pexp_poly (e, ct_opt) ->
    expr (opt core_type env ct_opt) e
  | Pexp_object clstr (* object ... end *) ->
    class_structure env clstr
  | Pexp_pack me (* (module ME) *)
    -> let _sign, _env = module_expr env me in env
  | Pexp_open (_override_flag,name,e)
        (* M.(E), let open M in E, let! open M in E *)
    -> let env = expr (do_open env name) e in
      Resolver.{ env with Env.unresolved = Unresolved.up env.Env.unresolved }
  | Pexp_constant _ | Pexp_extension _ (* [%ext] *) | Pexp_unreachable (* . *)
    -> env
and pattern env pat = match pat.ppat_desc with
  | Ppat_constant _ (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Ppat_interval _ (* 'a'..'z'*)
  | Ppat_any
  | Ppat_extension _
  | Ppat_var _ (* x *) -> env

  | Ppat_exception pat (* exception P *)
  | Ppat_lazy pat (* lazy P *)
  | Ppat_alias (pat,_) (* P as 'a *) -> pattern env pat

  | Ppat_array patterns (* [| P1; ...; Pn |] *)
  | Ppat_tuple patterns (* (P1, ..., Pn) *) ->
    List.fold_left pattern env patterns

  | Ppat_construct (c, p)
        (* C                None
           C P              Some P
           C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
        *) ->
    opt pattern (access env c) p
  | Ppat_variant (_, p) (*`A (None), `A P(Some P)*) ->
    opt pattern env p
  | Ppat_record (fields, _flag)
        (* { l1=P1; ...; ln=Pn }     (flag = Closed)
           { l1=P1; ...; ln=Pn; _}   (flag = Open)
        *) ->
    List.fold_left (fun env (_,p) -> pattern env p ) env fields
  | Ppat_or (p1,p2) (* P1 | P2 *) ->
    pattern (pattern env p1) p2
  | Ppat_constraint (pat, ct)  (* (P : T) *) ->
    pattern (core_type env ct) pat
  | Ppat_type name (* #tconst *) -> access env name
  | Ppat_unpack _s -> assert false
        (* (module P)
           Note: (module P : S) is represented as
           Ppat_constraint(Ppat_unpack, Ptyp_package)
         *)
(*  | Ppat_open (m,p) (* M.(P) *) ->
    Resolver.(up env.Env.signature) @@ pattern (do_open env m) p *)

and type_declaration env td  = Env.refocus env @@
  let env = List.fold_left (fun env (_,t,_) -> core_type env t) env td.ptype_cstrs in
  let env = type_kind env td.ptype_kind in
  opt core_type env td.ptype_manifest
and type_kind env = function
  | Ptype_abstract | Ptype_open -> env
  | Ptype_variant constructor_declarations ->
      List.fold_left constructor_declaration env constructor_declarations
  | Ptype_record label_declarations ->
    List.fold_left label_declaration env label_declarations
and constructor_declaration env cd =
  opt core_type (constructor_args env cd.pcd_args) cd.pcd_res
and constructor_args env = function
    | Pcstr_tuple cts -> List.fold_left core_type env cts
    | Pcstr_record lds -> List.fold_left label_declaration env lds
and label_declaration env ld = core_type env ld.pld_type
and type_extension env tyext =
  let env = access env tyext.ptyext_path in
  List.fold_left extension_constructor env tyext.ptyext_constructors
and core_type env ct =   match ct.ptyp_desc with
  | Ptyp_any  (*  _ *)
  | Ptyp_extension _ (* [%id] *)
  | Ptyp_var _ (* 'a *) -> env
  | Ptyp_arrow (_, t1, t2) (* [~? ]T1->T2 *) ->
      core_type (core_type env t1) t2
  | Ptyp_tuple cts (* T1 * ... * Tn *) ->
    List.fold_left core_type env cts
  | Ptyp_class (name,cts)
  | Ptyp_constr (name,cts) (*[|T|(T1n ..., Tn)] tconstr *) ->
    List.fold_left core_type (access env name) cts
  | Ptyp_object (lbls, _ ) (* < l1:T1; ...; ln:Tn[; ..] > *) ->
    List.fold_left (fun env (_,_,t) -> core_type env t) env lbls
  | Ptyp_poly (_, ct)
  | Ptyp_alias (ct,_) (* T as 'a *) -> core_type env ct

  | Ptyp_variant (row_fields,_,_labels) ->
    List.fold_left row_field env row_fields
  | Ptyp_package s (* (module S) *) ->
    Warning.confused(); env

and row_field env = function
  | Rtag (_,_,_,cts) -> List.fold_left core_type env cts
  | Rinherit ct -> core_type env ct
and package_type env (s,constraints) =
  List.fold_left (fun env (_,ct) -> core_type env ct)
    (access env s) constraints
and case env cs =
  pattern env cs.pc_lhs
  |> flip (opt expr) cs.pc_guard
  |> flip expr cs.pc_rhs
and do_include env incl =
    do_include_gen value module_expr env incl.pincl_mod
and extension_constructor env extc = match extc.pext_kind with
| Pext_decl (args, cto) ->
  opt core_type (constructor_args env args) cto
| Pext_rebind name -> access env name
and class_type env ct = match ct.pcty_desc with
  | Pcty_constr (name, cts ) (* c ['a1, ..., 'an] c *) ->
    List.fold_left core_type (access env name) cts
  | Pcty_signature cs (* object ... end *) -> class_signature env cs
  | Pcty_arrow (_arg_label, ct, clt) (* ^T -> CT *) ->
    class_type (core_type env ct) clt
  | Pcty_extension _ (* [%ext] *) -> env
and class_signature env cs = List.fold_left class_type_field env cs.pcsig_fields
and class_type_field env ctf = match ctf.pctf_desc with
  | Pctf_inherit ct -> class_type env ct
  | Pctf_val ( _, _, _, ct) (*val x : T *)
  | Pctf_method (_ ,_,_,ct) (* method x: T *)
    -> core_type env ct
  | Pctf_constraint  (t1, t2) (* constraint T1 = T2 *) ->
    core_type (core_type env t1) t2
  | Pctf_attribute _
  | Pctf_extension _ -> env
and class_structure env ct =
  List.fold_left class_field env ct.pcstr_fields
and class_field env field = match field.pcf_desc with
  | Pcf_inherit (_override_flag, ce, _) (* inherit CE *) ->
    class_expr env ce
  | Pcf_method (_, _, cfk)
  | Pcf_val (_,_, cfk) (* val x = E *)->
    class_field_kind env cfk
  | Pcf_constraint (_ , ct) (* constraint T1 = T2 *) ->
    core_type env ct
  | Pcf_initializer e (* initializer E *) -> expr env e
  | Pcf_attribute _
  | Pcf_extension _ -> Warning.extension (); env
and class_expr env ce = match ce.pcl_desc with
  | Pcl_constr (name, cts)  (* ['a1, ..., 'an] c *) ->
    List.fold_left core_type (access env name) cts
  | Pcl_structure cs (* object ... end *) -> class_structure env cs
  | Pcl_fun (_arg_label, eo, pat, ce)
        (* fun P -> CE                          (Simple, None)
           fun ~l:P -> CE                       (Labelled l, None)
           fun ?l:P -> CE                       (Optional l, None)
           fun ?l:(P = E0) -> CE                (Optional l, Some E0)
        *)
    -> opt expr env eo |> flip pattern pat |> flip class_expr ce
  | Pcl_apply (ce, les )
        (* CE ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           Invariant: n > 0
        *) ->
    List.fold_left (fun env (_,e) -> expr env e) (class_expr env ce) les
  | Pcl_let (_, value_bindings, ce ) (* let P1 = E1 and ... and Pn = EN in CE *)
    -> List.fold_left value_binding env value_bindings
  |> flip class_expr ce
  | Pcl_constraint (ce, ct) ->
    class_type (class_expr env ce) ct
  | Pcl_extension _ext -> Warning.extension () ; env
and class_field_kind env = function
  | Cfk_virtual ct -> core_type env ct
  | Cfk_concrete (_, e) -> expr env e
and class_declaration env cd = class_expr env (cd.pci_expr)
and class_type_declaration env ctd = class_type env (ctd.pci_expr)
and module_expr (env: Resolver.Env.t) mexpr :
  Resolver.Unresolved.focus * Resolver.Module.signature  =
  let open Resolver in
  match mexpr.pmod_desc with
  | Pmod_ident name ->
    find_signature value env name
  | Pmod_structure str ->
    let env' = (structure (enter_module env) str) in
    Env.( env'.unresolved,
          Module.Sig env.signature)
        (* struct ... end *)
  | Pmod_functor (name, sign, mex) ->
    let name = txt name in
      begin match sign with
        | Some s ->
          let unresolved, s = module_type env s in
          let unresolved, result = module_expr { env with Env.unresolved } mex in
          unresolved,
          Module.(Fun {
            arg = {Module.name;signature=s};
            result })
        | None -> assert false
      end
  | Pmod_apply (f,x) ->
    let unresolved, f = module_expr env f in
    let unresolved, x = module_expr {env with Env.unresolved} x in
    begin match f with
      | Module.Fun fn ->  unresolved, Module.apply env fn x
      | Module.Alias u ->
        let u = Unresolved.( (fun u -> Path.F u) $ u ) in
        Unresolved.(add_new (Extern u) unresolved), Module.Alias u
      | _ -> assert false
    end
        (* ME1(ME2) *)
  | Pmod_constraint (_module_expr,mt) ->
      module_type env mt
  | Pmod_unpack _expression -> assert false
        (* (val E) *)
  | Pmod_extension _extension ->
    Warning.extension();
    Env.unresolved env, Module.(Sig empty_sig)
        (* [%id] *)
and value_binding env vb =
  expr env vb.pvb_expr
and module_binding env (pmb_name, pmb_expr) =
  let open Resolver in
  let unresolved, sign = module_expr (enter_module env) pmb_expr in
  let md = {Module.signature = sign;
            name = txt pmb_name } in
  Resolver.bind value { env with Env.unresolved } md
and module_type env mt =
  let open Resolver in
  match mt.pmty_desc with
  | Pmty_signature s (* sig ... end *) -> signature env s
  | Pmty_functor (name, arg, res) (* functor(X : MT1) -> MT2 *) ->
    begin match arg with
      | None -> assert false (*??*)
      | Some arg ->
        let unr, sign = module_type (enter_module env) arg in
        let env =  { env with Env.unresolved = unr } in
        let unr', sign'= module_type (enter_module env) res in
        unr', Module.(Fun { arg = {name=txt name;signature=sign}; result= sign'})
    end

  | Pmty_with (mt, wlist) (* MT with ... *) ->
    assert false
  | Pmty_typeof me (* module type of ME *) ->
    module_expr env me
  | Pmty_extension _ ->
    Warning.extension();
    Resolver.( Env.unresolved env, Module.( Sig empty_sig ) )
        (* [%id] *)
  | Pmty_alias lid -> find_signature value env lid
        (* (module M) *)
  | Pmty_ident lid (* S *) ->
    Warning.confused ();
    find_signature type_ env lid

and module_declaration env mdec =
  let open Resolver in
  let u, s = module_type env mdec.pmd_type in
  let s = { Module.name =  txt mdec.pmd_name; signature = s} in
  let env = Env.{ env with unresolved = Unresolved.refocus_on env.unresolved u } in
  bind value env s
and module_type_declaration env mdec =
  let open Resolver in
  let open Option in
  let name = txt mdec.pmtd_name in
  let u, s = mdec.pmtd_type >>| begin fun mtd ->
      let u, s = module_type env mtd in
      u ,{ Module.name; signature = s}
    end
  >< (Env.unresolved env, Module.{ name; signature = Sig empty_sig} )
  in
  let env = Env.{ env with unresolved = Unresolved.refocus_on env.unresolved u } in
  bind type_ env s
and signature env sign =
  let env = List.fold_left signature_item (Resolver.enter_module env) sign in
  Resolver.( Env.unresolved env, Module.Sig env.Env.signature  )
and signature_item env item =  match item.psig_desc with
  | Psig_value vd (* val x: T *) ->
    core_type env vd.pval_type
  | Psig_type (_rec_flag, tds) (* type t1 = ... and ... and tn = ... *) ->
    List.fold_left type_declaration env tds
  | Psig_typext te (* type t1 += ... *) ->
    type_extension env te
  | Psig_exception ec (* exception C of T *) ->
    extension_constructor env ec
  | Psig_module md (* module X : MT *) ->
    module_declaration env md
  | Psig_recmodule mds (* module rec X1 : MT1 and ... and Xn : MTn *) ->
    let env' = List.fold_left module_declaration env mds in
    let env'' = Env.{ env with modules = env'.modules; types = env'.types } in
    Warning.confused ();
    List.fold_left module_declaration env'' mds
  | Psig_modtype mtd (* module type S = MT *) ->
    module_type_declaration env mtd
  | Psig_open od (* open X *) ->
    do_open env od.popen_lid
  | Psig_include id (* include MT *) ->
    do_include_gen type_ module_type env id.pincl_mod
  | Psig_class cds (* class c1 : ... and ... and cn : ... *) ->
    List.fold_left class_description env cds
  | Psig_class_type ctds ->
    List.fold_left class_type_declaration env ctds
  | Psig_attribute _ -> env
  | Psig_extension _ -> Warning.extension(); env
and class_description x = class_type_declaration x
and recmodules env mbs =
  let modules = List.fold_left
      (fun env md -> module_binding env (md.pmb_name,md.pmb_expr) ) in
  let env'= modules env mbs in
  let env' =
    Env.{ env' with signature = env.signature; unresolved=env'.unresolved } in
  modules env' mbs

let print_env env =
  let open Resolver in
  env.Env.unresolved.Unresolved.map
  |> Format.printf "@[%a@]@." Unresolved.pp

(*
let () =
  let open Resolver in
  let env = Env.empty in
  env
  |> open_ (Path.A "A")
  |> access (Path.A "B")
  |> open_ (Path.F {fn = Path.A "C"; arg = Path.A "D" } )
  |> print_env
*)

let () =
  structure Resolver.Env.empty ast
  |> print_env
