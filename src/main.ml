
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

open Parsetree
let txt x= x.Location.txt

let access env path = let open Option in
     path |> txt |> module_path >>| (fun p -> Resolver.access p env)
  >< env

let do_open env popen_lid =
  let path = from_lid @@ txt popen_lid in
  Resolver.open_ path env

let opt f env x=  Option.( x >>| (fun x -> f env x) >< env )
let flip f x y = f y x

let rec structure env str =
  List.fold_left structure_item env str
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
and expr env exp = match exp.pexp_desc with
  | Pexp_ident name (* x, M.x *) ->
    access env name
  | Pexp_let (_rec_flag, value_bindings, exp )
        (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
        *)
    ->
    let env = List.fold_left value_binding env value_bindings in
    expr env exp
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
    let env = opt expr env expr_opt in
    let env = pattern env pat in
    expr env expression
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
    expr (type_ env t) e
  | Pexp_coerce (e, t_opt, coer)
  (* (E :> T)        (None, T)
     (E : T0 :> T)   (Some T0, T)
  *) ->
    expr (opt type_ (type_ env coer) t_opt) e
  | Pexp_new name (* new M.c *) ->
    access env name
  | Pexp_setinstvar (_x, e) (* x <- e *) ->
    expr env e
  | Pexp_override labels (* {< x1 = E1; ...; Xn = En >} *) ->
    List.fold_left (fun env (_,e) -> expr env e) env labels
  | Pexp_letmodule (m, me, e) (* let module M = ME in E *) ->
    let env' = module_binding env (m,me) in
    let env' = expr env' e in
    Resolver.(up env.Env.signature env')
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

and type_declaration env tyd = env
and type_extension env ty_ext = env
and type_ env t = env
and core_type env ct = env
and case env cs = env
and exception_ env exn = env
and do_include env incl = env
and extension_constructor env extc = env
and recmodules env mbs = env
and module_type_declaration env mdec = env
and class_structure env ct = env
and class_declaration env c = env
and class_type_declaration env ct = env
and module_expr (env: Resolver.Env.t) mexpr :
  Resolver.Unresolved.focus * Resolver.Module.signature  =
  let open Resolver in
  let unresolved = Env.unresolved env in
  match mexpr.pmod_desc with
  | Pmod_ident name ->
    let path = from_lid @@ txt name in
    begin match Env.find path env with
    | Some(Either.Left sign ) -> unresolved, sign
    | Some(Either.Right unk) -> Unresolved.(add_new (Extern unk) unresolved),
                                Module.Alias unk
    | None ->
      Unresolved.(add_new (Loc path) unresolved),
      Module.Alias (Unresolved.alias_with_context env.Env.unresolved path)
    end
  | Pmod_structure str ->
    let env = (structure (enter_module env) str) in
       Env.( env.unresolved, Module.Sig env.signature)
        (* struct ... end *)
  | Pmod_functor (name, sign, mex) ->
    let name = txt name in
      begin match sign with
        | Some s ->
          let unresolved, s = signature env s in
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
  | Pmod_constraint (_module_expr,module_type) ->
      signature env module_type
  | Pmod_unpack _expression -> assert false
        (* (val E) *)
  | Pmod_extension _extension -> assert false
        (* [%id] *)
and value_binding env vb =
  expr env vb.pvb_expr
and module_binding env (pmb_name, pmb_expr) =
  let open Resolver in
  let unresolved, sign = module_expr (enter_module env) pmb_expr in
  let md = {Module.signature = sign;
            name = txt pmb_name } in
  Resolver.bind { env with Env.unresolved } md
and signature env _sign = Resolver.( Env.unresolved env, Module.(Sig empty_sig) )

let print_env env =
  let open Resolver in
  env.Env.unresolved.Unresolved.map
  |> Format.printf "@[%a@]\n" Unresolved.pp

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
