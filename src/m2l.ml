
type resolved = private R
type 'a declaration = private D
type module_type_brand = private MT
type module_brand = private M

type 'a arg = { name:Name.t; signature:'a }

let pp_arg pp ppf = function
  | Some arg ->
    Pp.fp ppf "(%s:%a)" arg.name pp arg.signature
  | None -> Pp.fp ppf "()"


let pp_args pp ppf args = Pp.fp ppf "%a" (Pp.list ~sep:"→@," @@ pp_arg pp) args;
  if List.length args > 0 then Pp.fp ppf "→"



type ('a,'arg_kind) module_info = {
  name:Name.t;
  alias_of:Epath.t option;
  args: 'arg_kind arg option list;
  signature:'a
}

type 'a sign_expr = {
  signature: 'a;
  deletions: Name.set;
  equalities: (Npath.t * Npath.t) list
}

type module_ = (signature, signature) module_info
and signature = { modules: mdict; module_types: mdict }
and mdict = module_ Name.Map.t


type level = Module | Module_type

let pp_alias = Pp.opt Epath.pp

let pp_level ppf lvl =  Pp.fp ppf "%s" (match lvl with
    | Module -> "module"
    | Module_type -> "module type"
  )

let rec pp_module ppf {name;args;alias_of;signature} =
  Pp.fp ppf "%s%a:%a@[<hv>[@,%a@,]@]"
    name pp_alias alias_of (pp_args pp_signature) args pp_signature signature
and pp_signature ppf {modules; module_types} =
  Pp.fp ppf "@[<hv>%a@]" pp_mdict modules;
  if Name.Map.cardinal module_types >0 then
    Pp.fp ppf "@,module types: @,@[<hv>%a@]"
      pp_mdict module_types
  else Pp.fp ppf " "
and pp_mdict ppf dict =
  Pp.fp ppf "%a" (Pp.list ~sep:" " pp_pair) (Name.Map.bindings dict)
and pp_pair ppf (_,md) = pp_module ppf md

let empty = Name.Map.empty
let empty_sig =
  {modules = empty; module_types = empty }

let sig_card s =
  let card = Name.Map.cardinal in
  card s.modules + card s.module_types

let (|+>) m x = Name.Map.add x.name x m
let add_module sg x = { sg with modules = sg.modules |+> x }
let add_module_type sg x = { sg with module_types = sg.module_types |+> x }


module Definitions = struct
  (** invariant defined ⊂ visible *)
  type definitions = { defined: signature; visible: signature }

  let empty_defs = { defined = empty_sig; visible = empty_sig }

  type full_defs = { args: signature arg option list; result:signature }
  let empty_full = { args = []; result=empty_sig }
  let simple defs = { args = []; result = defs }

  let only_visible defs =
    let diff = Name.Map.merge (fun _k x y ->
        match x, y with
        | Some _, (None|Some _) -> None
        | None , Some y -> Some y
        | None, None -> None ) in
    let d = defs.defined and v = defs.visible in
    { modules = diff d.modules v.modules;
      module_types = diff d.module_types v.module_types
    }

  let pp_defs ppf x = Pp.fp ppf "defined:@[[@,%a@,]@]"
      pp_signature x.defined;
    let v = only_visible x in
    if sig_card v > 0 then
      Pp.fp ppf "@, visible:@[@,[%a@,]@]"
        pp_signature v
    else ()

  let pp_fulldefs ppf x =
    if x.args = [] then pp_signature ppf x.result
    else Pp.fp ppf "%a@,→%a"
        (Pp.list ~sep:"→" @@ pp_arg pp_signature) x.args
        pp_signature x.result


  let no_arg x = {args = []; result = x }
  let sg_bind sign = { defined = sign; visible = sign }
  let sg_see sign = { empty_defs with visible = sign }

  let clear_visible defs = { defs with visible = defs.defined }

  let to_module name alias_of {args;result} =
    {name;alias_of; args; signature = result }

  let of_module {args;signature;_} = {result=signature;args}

  let to_sign fdefs =
    if fdefs.args <> [] then
      Error.signature_expected ()
    else
      fdefs.result

  let to_defs fdefs = sg_bind (to_sign fdefs)


  let merge s1 s2 =
    { modules = Name.Map.union' s1.modules s2.modules
    ; module_types = Name.Map.union' s1.module_types s2.module_types
    }

  let mod_def m =
    sg_bind { modules = empty |+> m; module_types = empty }

  let mod_defs ms =
    sg_bind { modules = List.fold_left (|+>) empty ms; module_types = empty }

  let sig_def m =
    sg_bind { module_types = empty |+> m; modules = empty }

  let sig_defs ms =
    sg_bind { module_types = List.fold_left (|+>) empty ms; modules = empty }

  let gen_def lvl = match lvl with
    | Module -> mod_def
    | Module_type -> sig_def

  let (+@) = merge
  let ( +| ) d1 d2 =
    { defined = d1.defined +@ d2.defined;
      visible = d1.visible +@ d2.visible }

  let bind_module md defs =
    { visible = { defs.visible with modules = defs.visible.modules |+> md };
      defined = { defs.defined with modules = defs.visible.modules |+> md }
    }

  let see_module md defs =
    { defs with
      visible = { defs.visible with modules = defs.visible.modules |+> md };
    }

  let bind_sign md defs =
    { visible = { defs.visible with modules = defs.visible.module_types |+> md };
      defined = { defs.defined with modules = defs.visible.module_types |+> md }
    }

  let bind level = match level with
    | Module -> bind_module
    | Module_type -> bind_sign

  let gen_defs l =
    List.fold_left (fun defs (level,md) -> bind level md defs) empty_defs l



  let arg_to_module ({name;signature}: _ arg) : module_ =
    { name; signature; args = []; alias_of = None }

  let is_functor = function
    | { args = []; _ } -> false
    |  _ -> true
end
open Definitions

type expression =
  | Defs of definitions (** Resolved module actions M = … / include … / open … *)
  | Open of Npath.t (** open A.B.C path *)
  | Include of module_expr
  | SigInclude of module_type
  (** include (M : s with module m := (deletions)
      and module m.k = n.w (equalities) *)
  | Bind of (level * bind)
  | Bind_rec of bind list
  | Minor of annotation
and annotation =
  { access: Name.set (** M.x ⇒ Name M *)
  ; values: m2l list (** let open in ...; let module M = .. *)
  ; opaques: module_expr list
  }
and bind = { name:Name.t; expr: module_expr }
and module_expr =
  | Resolved of full_defs
  | Ident of Npath.t
  | Apply of {f: module_expr; x:module_expr}
  | Fun of { arg: module_type arg option; body:module_expr }
  | Constraint of module_expr * module_type
  | Str of m2l
  | Opaque of m2l
and module_type =
  | Resolved of full_defs
  | Ident of Epath.t
  | Sig of m2l
  | Fun of { arg: module_type arg option; body:module_type }
  | With of {
      body: module_type;
      deletions: Name.set
      (* ; equalities: (Npath.t * Epath.t) list *)
    }
  | Of of module_expr
  | Opaque
and m2l = expression list

let empty_annot = { access=Name.Set.empty; values = []; opaques = [] }
let merge_annot a1 a2 =
  { access= Name.Set.union a1.access a2.access;
    values = a1.values @ a2.values;
    opaques = a1.opaques @ a2.opaques
  }

let demote_str halt arg =
  match arg with
  | None -> Fun { arg=None; body = halt }
  | Some ({name;signature}: _ arg) ->
    Fun { arg = Some {name; signature=Sig [Defs signature]}; body=halt }

let demote_sig halt arg : module_type =
  match arg with
  | None -> Fun { arg=None; body = halt }
  | Some ({name;signature}: _ arg) ->
    Fun { arg = Some {name; signature=Sig [Defs signature]}; body=halt }



let rec pp_expression ppf = function
  | Defs defs -> Pp.fp ppf "define %a" pp_defs defs

  | Minor {access;values; opaques} ->
    Pp.fp ppf "(%a@,%a@,%a)"
      pp_access access
      (Pp.opt_list ~sep:"" ~pre:"values: " pp) values
      (Pp.opt_list ~sep:"" ~pre:"opaques: " pp_opaque) opaques
  | Open epath -> Pp.fp ppf "@[<hv>open %a@]" Npath.pp epath
  | Include me -> Pp.fp ppf "@[<hv>include [%a]@]" pp_me me
  | SigInclude mt -> Pp.fp ppf "@[<hv>include type [%a]@]" pp_mt mt

  | Bind (level, bind ) -> pp_bind level ppf bind
  | Bind_rec bs ->
    Pp.fp ppf "rec@[<hv>[ %a ]@]"
      (Pp.list ~sep:"@, and @," @@ pp_bind Module ) bs
and pp_access ppf s =  if Name.Set.cardinal s = 0 then () else
    Pp.fp ppf "access:@[<hv>%a@]" Name.Set.pp s
and pp_opaque ppf me = Pp.fp ppf "⟨%a⟩" pp_me me
and pp_bind level ppf {name;expr} =
  Pp.fp ppf "@[%a %s =@,@[<hv>%a@] @]" pp_level level name pp_me expr
and pp_me ppf = function
  | Resolved fdefs -> pp_fulldefs ppf fdefs
  | Ident np -> Npath.pp ppf np
  | Str m2l -> Pp.fp ppf "@,struct@, %a end" pp m2l
  | Apply {f;x} -> Pp.fp ppf "%a(@,%a@,)" pp_me f pp_me x
  | Fun { arg; body } -> Pp.fp ppf "%a@,→%a" (pp_arg pp_mt) arg pp_me body
  | Constraint (me,mt) -> Pp.fp ppf "%a: @,%a" pp_me me pp_mt mt
  | Opaque m2l -> Pp.fp ppf "⟨%a⟩" pp m2l
and pp_mt ppf = function
  | Resolved fdefs -> pp_fulldefs ppf fdefs
  | Ident np -> Epath.pp ppf np
  | Sig m2l -> Pp.fp ppf "@,sig@, %a end" pp m2l
  | Fun { arg; body } ->  Pp.fp ppf "(%a)@,→%a" (pp_arg pp_mt) arg pp_mt body
  | With {body; deletions} ->
    Pp.fp ppf "%a@,/%a" pp_mt body Name.Set.pp deletions
  | Of me -> Pp.fp ppf "module type of@, %a" pp_me me
  | Opaque -> Pp.fp ppf "⟨⟩"
and pp ppf = Pp.fp ppf "@[<hv2>[@,%a@,]@]" (Pp.list ~sep:" " pp_expression)

type t = m2l

(*
let is_constant_str = function
  | Resolved fdefs -> true
  | [ Defs _ ] -> true
  | _ -> false
*)
let cdefs = function
  | (Resolved fdefs:module_expr) -> Some fdefs
  | _ -> None

let sig_cdefs = function
  | (Resolved fdefs:module_type) -> Some fdefs
  | _ -> None

let args_cdefs args =
  let open Option in
  let rec extract acc (args: module_type arg list) =
    acc >>= fun arg_defs ->
    begin match args with
      | [] -> Some arg_defs
      | {name;signature} :: args ->
        sig_cdefs signature >>= fun defs ->
        let md: signature arg = { name ; signature = to_sign defs } in
        extract (Some(md :: arg_defs)) args
    end
  in
  extract (Some []) args >>| List.rev

let halt l = false, l
let continue l = true, l

module Normalize = struct

  let (+:) x (more,l) =
    more, x :: l

  let rec all : m2l -> bool * m2l  = function
    | Defs d1 :: Defs d2 :: q ->
      all @@ Defs (  d1 +| d2) :: q
    | Defs d :: q ->
      let more, q = all q in more, Defs d :: q
    | Minor m :: q -> Minor (minor m) +: all q
    | (Open _ | Include _ | SigInclude _ | Bind _ | Bind_rec _) :: _ as l ->
      halt l
    | [] -> halt []
  and minor v =
    List.fold_left value { v with values = [] } v.values
  and value mn p =
    match snd @@ all p with
    | [] -> mn
    | Minor m :: q ->
      let mn = merge_annot mn m in
          { mn with values = q :: mn.values }
    | l -> { mn with values = l :: mn.values }

end

module Work = struct
  type ('a,'b) t = Halted of 'a | Done of 'b

  let is_done = function Done _ -> true | Halted _ -> false

  let all_done undone l =
    if List.for_all is_done l then
      Done (List.map (function Done x -> x | _ -> assert false ) l)
    else
      Halted (List.map
                (function Done d -> undone d
                        | Halted h -> h ) l)

  let fmap f g = function
    | Halted x -> Halted (f x)
    | Done r -> Done (g r)

  let fmap_done f = function
    | Halted _ as h  -> h
    | Done r -> Done (f r)


end

module Reduce = struct
open Work

  let minor m =
    if m.access = Name.Set.empty && m.values = [] then
      Done None
    else
      Halted (Minor m)


  let rec module_expr: module_expr -> (module_expr,full_defs) t  = function
    | Opaque _ | Ident _  as i -> Halted i
    | Apply {f;x} ->
      begin match module_expr f, module_expr x with
        | Done f, Done _ -> Done f
        | Halted f, Halted x -> Halted (Apply {f;x} )
        | Halted f, Done d -> Halted (Apply {f; x = Resolved d})
        | Done _, _ -> assert false
      end
    | Fun {arg;body} -> functor_expr [] arg body (** todo *)
    | Str [] -> Done empty_full
    | Str[Defs d] -> Done (no_arg d.defined)
    | Resolved d -> Done d
    | Str _ as i -> Halted i
    | Constraint(me,mt) ->
      constraint_ me mt
  and constraint_ me mt =
    match module_expr me, module_type mt with
        |  Done _, (Done _ as r) -> r
        | Done me, Halted mt ->
          Halted (Constraint(Resolved me, mt) )
        | Halted me, Done mt ->
          Halted (Constraint(me, Resolved mt) )
        | Halted me, Halted mt -> Halted ( Constraint(me,mt) )
  and module_type = function
    | Sig [] -> Done empty_full
    | Sig [Defs d] -> Done (no_arg d.defined)
    | Resolved d -> Done d
    | Sig _ as s -> Halted s
    | Ident _ | With _ as mt -> Halted mt
    | Fun _ -> Error.include_functor () (** todo *)
    | Of me -> of_ (module_expr me)
    | Opaque -> Halted (Opaque:module_type)
  and of_ = function
    | Halted me -> Halted (Of me)
    | Done d -> Done d
  and functor_expr args arg body =
    let ex_arg =
      match arg with
      | None -> Done None
      | Some arg ->
        match module_type arg.signature with
        | Halted h -> Halted (Some {name = arg.name; signature = h })
        | Done d   -> Done (Some{name=arg.name; signature = to_sign d}) in
    match ex_arg with
    | Halted me -> Halted (List.fold_left demote_str (Fun {arg=me;body}) args )
    | Done arg ->
      match module_expr body with
      | Done {args; result} -> Done {args = arg :: args; result }
      | Halted me ->
        let arg =
          Option.( arg >>| fun arg ->
                   { name = arg.name;
                     signature:module_type= Resolved (no_arg arg.signature) }
                 ) in
        Halted (List.fold_left demote_str (Fun {arg;body=me}) args)


  let (%>) f g x = x |> f |> g

  let some x = Some x

  let gen_include unbox box i = match unbox i with
    | Halted h -> Halted (box h)
    | Done fdefs ->
      let defs = to_defs fdefs in
      Done (Some defs)

  let include_ module_expr = gen_include module_expr (fun i -> Include i)
  let sig_include module_type = gen_include module_type (fun i -> SigInclude i)


  let bind module_expr lvl {name;expr} =
    match module_expr expr with
    | Halted h -> Halted ( Bind(lvl, {name; expr = h} ) )
    | Done d ->
      let m = Definitions.to_module name None d (* lost aliasing? *) in
      Done (Some(Definitions.gen_def lvl m))

  let bind_rec bs =
    let pair x y = x,y in
    let mapper {name;expr} = expr |> module_expr |> fmap (pair name) (pair name) in
    let bs = List.map mapper bs in
    let undone (name,defs) = name, Resolved defs in
    let recombine (name,expr) = {name;expr} in
    match all_done undone bs with
    | Halted bs -> Halted (Bind_rec (List.map recombine bs))
    | Done defs ->
      let defs =
        List.fold_left
          ( fun defs (name,d) -> bind_module (to_module name None d) defs )
          empty_defs defs in
      Done ( Some defs )


  let expr = function
    | (Defs _ | Open _ ) as d -> Halted d
    | Include i -> include_ module_expr i
    | SigInclude i -> sig_include module_type i
    | Bind (lvl, b) -> bind module_expr lvl b
    | Bind_rec bs -> bind_rec bs
    | Minor m -> minor m

end

module Build = struct
  let access path = Minor { empty_annot with
                            access = Name.Set.singleton @@ Epath.prefix path }
  let open_ path = Open path
  let value v = Minor { empty_annot with values = [v] }
  let opaque o = Minor { empty_annot with opaques = [o] }
end
