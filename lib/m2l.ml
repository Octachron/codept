
type resolved = private R
type 'a declaration = private D
type module_type_brand = private MT
type module_brand = private M

module M = Module
module S = Module.Sig
module Arg = M.Arg
module D = Definition
module Def = D.Def

module Partial = struct
  type t =
    { origin: Module.origin;
      args: Module.t option list;
      result:Module.signature }
  let empty = { origin = Submodule; args = []; result= S.empty }
  let simple defs = { empty with result = defs }
  let pp ppf (x:t) =
    if x.args = [] then M.pp_signature ppf x.result
    else Pp.fp ppf "%a@,→%a"
        M.pp_args x.args
        M.pp_signature x.result


  let no_arg x = { origin = Submodule; args = []; result = x }

  let drop_arg (p:t) = match  p.args with
    | _ :: args -> { p with args }
    | [] ->
      match p.origin with
      | Extern | First_class | Rec -> p (* we guessed the arg wrong *)
      | Unit | Submodule | Arg | Alias _ -> Error.not_a_functor ()
  (* there is an error somewhere *)

  let to_module ?origin name (p:t) =
    let origin = match origin with
      | Some o -> Module.at_most p.origin o
      | None -> p.origin
    in
    {M.name;origin; args = p.args; signature = p.result }

  let to_arg name (p:t) =
    {M.name;origin=Arg; args = p.args; signature = p.result }

  let of_module {M.args;signature;origin; _} = {origin;result=signature;args}

  let is_functor x = x.args <> []

  let to_sign fdefs =
    if fdefs.args <> [] then
      ( Pp.fp Pp.err "%a@." pp fdefs;
        Error.signature_expected ()
      )
    else
      fdefs.result

  let to_defs fdefs = Definition.sg_bind @@ to_sign fdefs

end

module P = Partial

type 'a bind = {name:Name.t; expr:'a}

type expression =
  | Defs of Definition.t (** Resolved module actions M = … / include … / open … *)
  | Open of Npath.t (** open A.B.C path *)
  | Include of module_expr
  | SigInclude of module_type
  (** include (M : s with module m := (deletions)
      and module m.k = n.w (equalities) *)
  | Bind of module_expr bind
  | Bind_sig of module_type bind
  | Bind_rec of module_expr bind list
  | Minor of annotation
  | Extension_node of extension
and annotation =
  { access: Name.set (** M.x ⇒ access \{M\} *)
  ; values: m2l list (** let open in ...; let module M = .. *)
  ; packed: module_expr list (** (module M) *)
  }
and module_expr =
  | Resolved of Partial.t
  (** already resolved module expression, generally
      used in subexpression when waiting for other parts
      of module expression to be resolved
  *)
  | Ident of Npath.t (** A.B… **)
  | Apply of {f: module_expr; x:module_expr} (** F(X) *)
  | Fun of module_expr fn (** functor (X:S) -> M *)
  | Constraint of module_expr * module_type (** M:S *)
  | Str of m2l (** struct … end *)
  | Val of annotation (** (val … ) *)
  | Extension_node of extension (** [%ext …] *)
  | Abstract (** □ *)
  | Unpacked (** (module M *)
and module_type =
  | Resolved of Partial.t (** same as in the module type *)
  | Alias of Npath.t (** module m = A…  *)
  | Ident of Epath.t (** module M : F(X).s
                         epath due to F.(X).s expression *)
  | Sig of m2l (** sig end*)
  | Fun of module_type fn (** functor (X:S) → M *)
  | With of {
      body: module_type;
      deletions: Name.set
      (* ; equalities: (Npath.t * Epath.t) list *)
    }
  | Of of module_expr
  | Extension_node of extension
  | Abstract
and extension = {name:string; extension:extension_core}
and extension_core = Module of m2l
              | Val of annotation
and m2l = expression list
and 'a fn = { arg: module_type Arg.t option; body:'a }

type arg = module_type Arg.t option

module Annot = struct
  type t = annotation
  let empty = { access=Name.Set.empty; values = []; packed = [] }
  let is_empty  = (=) empty
  let merge a1 a2 =
    { access= Name.Set.union a1.access a2.access;
      values = a1.values @ a2.values;
      packed = a1.packed @ a2.packed
    }

  let (++) = merge

  let union l =
    List.fold_left (++) empty l
  let union_map f l =
    List.fold_left (fun res x -> res ++ f x ) empty l

  let access name = { empty with
                      access = Name.Set.singleton name }
  let value v = { empty with values = v }
  let pack o ={ empty with packed = o }

  let opt f x = Option.( x >>| f >< empty )

end
let demote_str fn arg =
  let body = Fun fn in
  match arg with
  | None -> { arg=None; body }
  | Some ({name;signature}: _ Arg.t) ->
    { arg = Some {name; signature=Sig [Defs signature]}; body }

let demote_sig fn arg : module_type fn  =
  let body : module_type = Fun fn in
  match arg with
  | None -> { arg=None; body }
  | Some ({name;signature}: _ Arg.t) ->
    { arg = Some {name; signature=Sig [Defs signature]}; body }

let fn_sig arg : module_type = Fun arg
let fn arg : module_expr  = Fun arg

let rec pp_expression ppf = function
  | Defs defs -> Pp.fp ppf "define %a" D.pp defs

  | Minor annot -> pp_annot ppf annot
  | Open epath -> Pp.fp ppf "@[<hv>open %a@]" Npath.pp epath
  | Include me -> Pp.fp ppf "@[<hv>include [%a]@]" pp_me me
  | SigInclude mt -> Pp.fp ppf "@[<hv>include type [%a]@]" pp_mt mt

  | Bind bind -> pp_bind ppf bind
  | Bind_sig bind -> pp_bind_sig ppf bind
  | Extension_node e -> pp_extension ppf e
  | Bind_rec bs ->
    Pp.fp ppf "rec@[<hv>[ %a ]@]"
      (Pp.(list ~sep:(s "@, and @,")) @@ pp_bind ) bs
and pp_annot ppf {access;values; packed} =
  Pp.fp ppf "%a%a%a"
    pp_access access
    Pp.(opt_list ~sep:(s " @,") ~pre:(s "@,values: ") pp) values
    Pp.(opt_list ~sep:(s " @,") ~pre:(s "packed: ") pp_opaque) packed
and pp_access ppf s =  if Name.Set.cardinal s = 0 then () else
    Pp.fp ppf "access:@[<hv>%a@]" Name.Set.pp s
and pp_opaque ppf me = Pp.fp ppf "⟨%a⟩" pp_me me
and pp_bind ppf {name;expr} =
  match expr with
  | Constraint(Abstract, Alias np) ->
    Pp.fp ppf "@[module %s ≡ %a" name Npath.pp np
  | Constraint(Abstract, mt) ->
    Pp.fp ppf "@[module %s:@[<hv>%a@] @]" name pp_mt mt
  | Constraint(Unpacked, mt) ->
    Pp.fp ppf "@[(module %s:@[<hv>%a@])@]" name pp_mt mt
  | Unpacked ->
    Pp.fp ppf "(module %s)" name
  | _ ->
    Pp.fp ppf "@[module %s = @,@[<hv>%a@] @]" name pp_me expr
and pp_bind_sig ppf {name;expr} =
  match expr with
  | Alias id ->
    Pp.fp ppf "@[module type %s ≡ %a @]" name Npath.pp id
  | Abstract ->
    Pp.fp ppf "@[module type %s@]" name
  | _ ->
    Pp.fp ppf "@[module type %s = @,@[<hv>%a@] @]" name pp_mt expr
and pp_me ppf = function
  | Resolved fdefs -> Pp.fp ppf "✔%a" Partial.pp fdefs
  | Ident np -> Npath.pp ppf np
  | Str m2l -> Pp.fp ppf "@,struct@, %a end" pp m2l
  | Apply {f;x} -> Pp.fp ppf "%a(@,%a@,)" pp_me f pp_me x
  | Fun { arg; body } -> Pp.fp ppf "%a@,→%a" (Arg.pp pp_mt) arg pp_me body
  | Constraint (me,mt) -> Pp.fp ppf "%a: @,%a" pp_me me pp_mt mt
  | Val annot -> Pp.fp ppf "⟨val %a⟩" pp_annot annot
  | Extension_node ext -> Pp.fp ppf "%a" pp_extension ext
  | Abstract -> Pp.fp ppf "⟨abstract⟩"
  | Unpacked -> Pp.fp ppf "⟨unpacked⟩"
and pp_mt ppf = function
  | Resolved fdefs -> Pp.fp ppf "✔%a" Partial.pp fdefs
  | Alias np -> Pp.fp ppf "(≡)%a" Npath.pp np
  | Ident np -> Epath.pp ppf np
  | Sig m2l -> Pp.fp ppf "@,sig@, %a end" pp m2l
  | Fun { arg; body } ->  Pp.fp ppf "%a@,→%a" (Arg.pp pp_mt) arg pp_mt body
  | With {body; deletions} ->
    Pp.fp ppf "%a@,/%a" pp_mt body Name.Set.pp deletions
  | Of me -> Pp.fp ppf "module type of@, %a" pp_me me
  | Extension_node ext -> Pp.fp ppf "%a" pp_extension ext
  | Abstract -> Pp.fp ppf "⟨abstract⟩"
and pp_extension ppf x = Pp.fp ppf "[%%%s @[<hv>%a@]]" x.name pp_extension_core
    x.extension
and pp_extension_core ppf = function
  | Module m -> pp ppf m
  | Val m -> pp_annot ppf m
and pp ppf = Pp.fp ppf "@[<hv2>[@,%a@,]@]" Pp.(list ~sep:(s " @,") pp_expression)

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
  let rec extract acc (args: module_type Arg.t list) =
    acc >>= fun arg_defs ->
    begin match args with
      | [] -> Some arg_defs
      | {Arg.name;signature} :: args ->
        sig_cdefs signature >>= fun defs ->
        let md: _ Arg.t = { name ; signature = defs } in
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
      all @@ Defs Def.(  d1 +| d2) :: q
    | Defs d :: q ->
      let more, q = all q in more, Defs d :: q
    | Minor m :: q -> Minor (minor m) +: all q
    | (Open _ | Include _ | SigInclude _ | Bind_sig _ | Bind _ | Bind_rec _
      | Extension_node _ )
      :: _ as l ->
      halt l
    | [] -> halt []
  and minor v =
    List.fold_left value { v with values = [] } v.values
  and value mn p =
    match snd @@ all p with
    | [] -> mn
    | Minor m :: q ->
      let mn = Annot.merge mn m in
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

   let fmap_halted f = function
    | Halted h  -> Halted (f h)
    | Done _ as r -> r


end

module Reduce = struct
open Work

  let minor m =
    if m.access = Name.Set.empty && m.values = [] then
      Done None
    else
      Halted (Minor m)


  let rec module_expr: module_expr -> (module_expr,Partial.t) Work.t  = function
    | Abstract | Unpacked -> Done Partial.empty
    |  Val _ | Ident _  as i -> Halted i
    | Apply {f;x} ->
      begin match module_expr f, module_expr x with
        | Done f, Done _ -> Done (Partial.drop_arg f)
        | Halted f, Halted x -> Halted (Apply {f;x} )
        | Halted f, Done d -> Halted (Apply {f; x = Resolved d})
        | Done _, _ -> assert false
      end
    | Fun {arg;body} -> functor_expr [] arg body (** todo *)
    | Str [] -> Done Partial.empty
    | Str[Defs d] -> Done (Partial.no_arg d.defined)
    | Resolved d -> Done d
    | Str _ as i -> Halted i
    | Extension_node _ as e -> Halted e
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
    | Sig [] -> Done P.empty
    | Sig [Defs d] -> Done (P.no_arg d.defined)
    | Resolved d -> Done d
    | Sig _ as s -> Halted s
    | Ident _ | Alias _ | With _ as mt -> Halted mt
    | Fun _ -> Error.include_functor () (** todo *)
    | Of me -> of_ (module_expr me)
    | Abstract -> Halted (Abstract:module_type)
    | Extension_node _ as e -> Halted e
  and of_ = function
    | Halted me -> Halted (Of me)
    | Done d -> Done d
  and functor_expr args arg body =
    let ex_arg =
      match arg with
      | None -> Done None
      | Some arg ->
        match module_type arg.signature with
        | Halted h -> Halted (Some {Arg.name = arg.name; signature = h })
        | Done d   -> Done (Some(P.to_arg arg.name d)) in
    match ex_arg with
    | Halted me -> Halted (fn @@ List.fold_left demote_str {arg=me;body} args )
    | Done arg ->
      match module_expr body with
      | Done p -> Done { p with args = arg :: p.args }
      | Halted me ->
        let arg =
          Option.( arg >>| fun arg ->
                   { Arg.name = arg.name;
                     signature:module_type= Resolved (P.of_module arg) }
                 ) in
        Halted (fn @@ List.fold_left demote_str {arg;body=me} args)


  let (%>) f g x = x |> f |> g

  let some x = Some x

  let gen_include unbox box i = match unbox i with
    | Halted h -> Halted (box h)
    | Done fdefs ->
      let defs = P.to_defs fdefs in
      Done (Some defs)

  let include_ module_expr = gen_include module_expr (fun i -> Include i)
  let sig_include module_type = gen_include module_type (fun i -> SigInclude i)


  let bind module_expr {name;expr} =
    match module_expr expr with
    | Halted h -> Halted ( Bind( {name; expr = h} ) )
    | Done d ->
      let m = P.to_module name d in
      Done (Some(Def.md m))

  let bind_sig module_sig {name;expr} =
    match module_sig expr with
    | Halted h -> Halted ( Bind_sig( {name; expr = h} ) )
    | Done d ->
      let m = P.to_module name d in
      Done (Some(Def.sg m))


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
          ( fun defs (name,d) -> D.bind (P.to_module name d) defs )
          D.empty defs in
      Done ( Some defs )


  let expr = function
    | (Defs _ | Open _ | Extension_node _ ) as d -> Halted d
    | Include i -> include_ module_expr i
    | SigInclude i -> sig_include module_type i
    | Bind b -> bind module_expr b
    | Bind_sig b -> bind_sig module_type b
    | Bind_rec bs -> bind_rec bs
    | Minor m -> minor m

end

module Build = struct
  let access path = Minor (Annot.access @@ Epath.prefix path)
  let open_ path = Open path
  let value v = Minor ( Annot.value v)
  let pack o = Minor (Annot.pack o)
end
