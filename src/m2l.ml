
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
  Pp.fp ppf "%s%a:%a@[<hv 2> %a@]"
    name pp_alias alias_of (pp_args pp_signature) args pp_signature signature
and pp_signature ppf {modules; module_types} =
  Pp.fp ppf "@[<hv2> %a@]@, module types: @,@[<hv2>%a@] @]"
    pp_mdict modules pp_mdict module_types
and pp_mdict ppf dict =
  Pp.fp ppf "%a" (Pp.list ~sep:" @," pp_pair) (Name.Map.bindings dict)
and pp_pair ppf (_,md) = pp_module ppf md

let empty = Name.Map.empty
let empty_sig =
  {modules = empty; module_types = empty }

let (|+>) m x = Name.Map.add x.name x m

(** invariant defined ⊂ visible *)
type definitions = { defined: signature; visible: signature }

let pp_defs ppf x = Pp.fp ppf "defined:[@[%a@]]@, visible:[@[%a@]]"
    pp_signature x.defined pp_signature x.visible

let empty_defs = { defined = empty_sig; visible = empty_sig }
let sg_bind sign = { defined = sign; visible = sign }
let sg_see sign = { empty_defs with visible = sign }

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
let ( +| ) d1 d2 = { defined = d1.defined +@ d2.defined;
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

type expression =
  | Defs of definitions (** Resolved modules definitions M = … *)
  | Open of Epath.t (** open path *)
  | Include of module_expr
  | SigInclude of module_type
  (** include (M : s with module m := (deletions)
      and module m.k = n.w (equalities) *)
  | Bind of (level * bind)
  | Bind_rec of bind list

  | Access of Name.set (** M.x ⇒ Name M *)
  | Value of m2l
  | Opaque of module_expr (** first class module *)
and bind = { name:Name.t; expr: module_expr }
and module_expr =
  | Ident of Epath.t
  | Apply of {f: module_expr; x:module_expr}
  | Fun of { arg: module_type arg option; body:module_expr }
  | Constraint of module_expr * module_type
  | Str of m2l
  | Opaque of m2l
and module_type =
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

let rec pp_expression ppf = function
  | Defs defs -> Pp.fp ppf "define @[<hov2>%a@]@, " pp_defs defs
  | Access n -> Pp.fp ppf "access [%a]@, " Name.Set.pp n

  | Value m2l -> Pp.fp ppf "val [%a]" pp m2l
  | Opaque me -> Pp.fp ppf "⟨%a⟩" pp_me me

  | Open epath -> Pp.fp ppf "open %a @," Epath.pp epath
  | Include me -> Pp.fp ppf "include [%a]@," pp_me me
  | SigInclude mt -> Pp.fp ppf "include type [%a]@," pp_mt mt

  | Bind (level, bind ) -> pp_bind level ppf bind
  | Bind_rec bs ->
    Pp.fp ppf "rec@[[ %a ]@] @,"
      (Pp.list ~sep:"@, and @," @@ pp_bind Module ) bs
and pp_bind level ppf {name;expr} =
  Pp.fp ppf "%a %s =@, @[ %a @] " pp_level level name pp_me expr
and pp_me ppf = function
  | Ident np -> Epath.pp ppf np
  | Str m2l -> Pp.fp ppf "@,struct@, %a end" pp m2l
  | Apply {f;x} -> Pp.fp ppf "%a(@,%a@,)" pp_me f pp_me x
  | Fun { arg; body } -> Pp.fp ppf "%a@,→%a" (pp_arg pp_mt) arg pp_me body
  | Constraint (me,mt) -> Pp.fp ppf "%a: @,%a" pp_me me pp_mt mt
  | Opaque m2l -> Pp.fp ppf "⟨%a⟩" pp m2l
and pp_mt ppf = function
  | Ident np -> Epath.pp ppf np
  | Sig m2l -> Pp.fp ppf "@,sig@, %a end" pp m2l
  | Fun { arg; body } ->  Pp.fp ppf "(%a)@,→%a" (pp_arg pp_mt) arg pp_mt body
  | With {body; deletions} ->
    Pp.fp ppf "%a@,/%a" pp_mt body Name.Set.pp deletions
  | Of me -> Pp.fp ppf "module type of@, %a" pp_me me
  | Opaque -> Pp.fp ppf "⟨⟩"
and pp ppf = Pp.fp ppf "[@[%a@]]@." (Pp.list pp_expression)

type t = m2l

let is_constant = function
  | []
  | [ Defs _ ] -> true
  | _ -> false

let cdefs = function
  | [] -> Some empty_defs
  | [ Defs s  ] -> Some s
  | _ -> None

let args_cdefs args =
  let open Option in
  let rec extract acc (args: m2l arg list) =
    acc >>= fun arg_defs ->
    begin match args with
      | [] -> Some arg_defs
      | {name;signature} :: args ->
        cdefs signature >>= fun defs ->
        let md: signature arg = { name ; signature = defs.defined } in
        extract (Some(md :: arg_defs)) args
    end
  in
  extract (Some []) args >>| List.rev

module Normalize = struct


end

let go_on (_,s) = true, s
let stop (_,s) = false, s
(*
let rec normalize = function
  | Defs d1 :: Defs d2 :: q ->
    normalize @@ Defs ( d1 +| d2 ) :: q
  | Defs _ as d :: q ->
    let more, q = normalize q in
    if more then normalize (d :: q) else false, (d :: q)
  | Access a1 :: Access a2 :: l ->
    normalize ( Access (Name.Set.union a1 a2) :: l )
  | Access _ as a :: (Defs _ as d) :: l ->
    let _, q = normalize (a :: l) in
    go_on @@ normalize @@ d :: q
  | Access _ as a :: q ->
    let more, q = normalize q in
    if more then normalize (a::q) else false, a::q
  | Value [(Value _ as v)] :: q ->
    normalize @@ v :: q
  | Value s :: q ->
    let _, s = normalize s in
    if is_constant s then
      go_on @@ normalize q
    else
      false, Value s :: q
  | (Open _| Include _ ) :: _ as l -> false, l
  | Declaration (level, md) :: q as l ->
    let md = md_normalize md in
    begin match md_cdefs md with
    | Some d ->
        go_on @@ normalize @@ Defs ( gen_def level d )  :: l
    | None ->
      false, Declaration (level, md) :: q
    end
  | Module_rec mds :: q ->
    let mds = List.map md_normalize mds in
    let defs = Option.list_map md_cdefs mds in
    begin match defs with
      | Some defs ->  go_on @@ normalize @@ Defs (mod_defs defs) :: q
      | None -> false, Module_rec mds :: q
    end
  | [] -> false, []
and md_normalize (md: module_decl) : module_decl =
  let me = md.signature in
  let signature = snd @@ body_normalize me.signature in
  let structure = snd @@ body_normalize me.structure in
  let args = List.map arg_normalize md.args in
  let signature = {signature;structure} in
  { md with args; signature }
and body_normalize = function
  | Alias _ as a -> false, a
  | Explicit m2l ->
    let more, p = normalize m2l in
    more, Explicit p
and md_cdefs md =
  let me = md.signature in
  match body_cdefs me.signature, body_cdefs me.structure, args_cdefs md.args with
  | Some sg, Some _, Some args ->
    Some {name = md.name; alias_of = md.alias_of; signature = sg.defined; args }
  | _ -> None
and arg_normalize {name; signature } =
  { name ; signature = snd @@ normalize signature }

let rec find_module path (m:signature) =
  let open Epath in
  match path with
  | T -> raise Not_found
  | A n -> Name.Map.find n m.modules
  | S(p,n) ->
    let m' = find_module p m in
    Name.Map.find n  m'.signature.modules
  | F fn -> find_module fn m

exception Kind_error of string

let find_module_type path m =
  let open Epath in
  match path with
  | T -> raise Not_found
  | A n -> Name.Map.find n m.module_types
  | S(p,n) ->
    let m' = find_module p m in
    Name.Map.find n  m'.signature.module_types
  | F _ -> raise (Kind_error "Module type expected, got functor application")

let find = function
  | Module -> find_module
  | Module_type -> find_module_type

let open_ defs sgn  = { defs with visible = defs.visible +@ sgn }
let include_ defs sgn  = defs +| sg_bind sgn

let delete names sg =
  { sg with modules = Name.Set.fold Name.Map.remove names sg.modules }

let rec replace ~target ~alias sg = match target with
  | [] -> raise (Invalid_argument "M2l.replace with empty path")
  | [a] ->
    let md = Name.Map.find a sg.modules in
    let md = { md with alias_of = Some alias } in
    { sg with modules = sg.modules |+> md }
  | a :: q ->
    let md = Name.Map.find a sg.modules in
    let signature = replace ~target:q ~alias md.signature in
    let md = { md with signature } in
    { sg with modules = sg.modules |+> md }

let precise_alias (sg:signature) al =
  let sg = delete al.deletions sg in
  let sg = List.fold_left (fun sg (x,y) ->
      replace ~target:x ~alias:y sg) sg al.equalities in
  sg

let rec compute state = function
  | Defs d :: q -> Defs d :: compute ( state +| d ) q
  | Access s :: q ->
    let _, s' = Name.Set.partition
        (fun x -> Name.Map.mem x state.visible.modules) s in
    if Name.Set.cardinal s' = 0 then
      compute state q
    else
      Access s' :: compute state q
  | Value p' :: q  -> let p'' = compute state p' in
    if is_constant p'' then
      compute state q
    else
      Value p'' :: compute state q
  | Open path :: q as p ->
    begin
      match find_module path state.visible with
      | md -> Defs (sg_see md.signature) :: compute ( open_ state md.signature ) q
      | exception Not_found -> p
    end
  | Include me :: q ->
    let continue, p = normalize @@ Include (compute_me state me) :: q in
    if continue then compute state p else p
  | Declaration (level, md) :: q  ->
    let md = compute_md state md in
    normalize_and_continue state @@ Declaration ( level, md ) :: q
  | Module_rec mds :: q ->
    (** First stage: we mockup all modules bindings in the recursive signature X_1,…, X_n with signature sig end *)
    let mockup_args = List.map ( fun (arg: m2l arg) : signature arg ->
        { name =arg.name; signature = empty_sig } ) in
    let mockup_sig = List.map (fun md : module_ ->
        { name = md.name; alias_of = None; args = mockup_args md.args;
          signature = empty_sig } ) mds in
    let state' =
      List.fold_left (fun st md -> see_module md st ) state mockup_sig in
    let mds = List.map (compute_md state') mds in
    normalize_and_continue state' @@ Module_rec mds :: q
  | [] -> []
and compute_md state md =
  let args = List.map (fun (arg: m2l arg) ->
      { arg with signature = snd @@ normalize @@ compute state arg.signature} )
      md.args in
  let state' =
    match args_cdefs args with
    | Some args -> List.fold_left (fun defs arg ->
        see_module (arg_to_module arg) defs ) state args
    | None -> state in
  let me = compute_me state' md.signature in
  { md with args; signature = me }
and compute_me state me =
  { signature = compute_body state (snd @@ body_normalize @@ me.signature)
  ; structure = compute_body state (snd @@ body_normalize @@ me.structure) }
and compute_body state = function
  | Explicit mtl -> Explicit (compute state @@ snd @@ normalize @@ mtl)
  | Alias ae -> compute_alias state ae

and compute_alias state ae =
  match find ae.level ae.path state.visible with
  | md ->
    let sg = md.signature in
    let sg = precise_alias sg ae in
    Explicit [ Defs (sg_bind sg ) ]
  | exception Not_found -> Alias ae

and normalize_and_continue state p =
    let continue, p = normalize p in
    if continue then compute state p else p
*)

module Build = struct
  let access path = Access (Name.Set.singleton @@ Epath.prefix path )
  let open_ path = Open path
end
