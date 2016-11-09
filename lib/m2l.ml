
module M = Module
module S = Module.Sig

module Arg = M.Arg
module D = Definition
module Def = D.Def

module P = Partial_module

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
  { access: Name.set (** M.N.L.x ⇒ access \{M\} *)
  ; values: m2l list (** let open in ...; let module M = .. *)
  ; packed: module_expr list (** (module M) *)
  }
and module_expr =
  | Resolved of P.t
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
  | Open_me of { resolved: Definition.t; opens:Npath.t list; expr:module_expr}
  (** M.(…N.( module_expr)…)
      Note: used for pattern open. *)
and module_type =
  | Resolved of P.t (** same as in the module type *)
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
  | Of of module_expr (** module type of … *)
  | Extension_node of extension (** [%%… ] *)
  | Abstract
and extension = {name:string; extension:extension_core}
and extension_core =
  | Module of m2l
  | Val of annotation
and m2l = expression list
and 'a fn = { arg: module_type Arg.t option; body:'a }

type arg = module_type Arg.t option

module Block = struct
  let either x f y = if x = None then f y else x
  let first f l = List.fold_left (fun x y -> either x f y) None l

  let rec me = function
    | Resolved _ -> None
    | Ident n -> Some( List.hd n )
    | Apply {f; x} -> either (me f) me  x
    | Fun fn ->
      either
        Option.(fn.arg >>= fun {Arg.signature;_} -> mt signature)
        me fn.body
    | Constraint (e,t) -> either (me e) mt t
    | Str code -> m2l code
    | Val _ | Extension_node _ | Abstract | Unpacked -> None
    | Open_me {opens = []; expr; _ } -> me expr
    | Open_me {opens = a::_ ; _ } -> Some (List.hd a)
  and mt = function
    | Resolved _ -> None
    | Alias n -> Some (List.hd n)
    | Ident e -> Some (Epath.prefix e)
    | Sig code -> m2l code
    | Fun fn -> either
                  Option.(fn.arg >>= fun {Arg.signature;_} -> mt signature)
                  mt fn.body
    | With { body; _ } -> mt body
    | Of e -> me e
    | Extension_node _
    | Abstract -> None
  and expr = function
    | Defs _ -> None
    | Open p -> Some (List.hd p)
    | Include e -> me e
    | SigInclude t -> mt t
    | Bind {expr;_} -> me expr
    | Bind_sig {expr;_} -> mt expr
    | Bind_rec l ->
      first (fun b -> me b.expr) l
    | Minor _ -> None
    | Extension_node _ -> None
  and m2l code = first expr code
end

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
  | Resolved fdefs -> Pp.fp ppf "✔%a" P.pp fdefs
  | Ident np -> Npath.pp ppf np
  | Str m2l -> Pp.fp ppf "@,struct@, %a end" pp m2l
  | Apply {f;x} -> Pp.fp ppf "%a(@,%a@,)" pp_me f pp_me x
  | Fun { arg; body } -> Pp.fp ppf "%a@,→%a" (Arg.pp pp_mt) arg pp_me body
  | Constraint (me,mt) -> Pp.fp ppf "%a: @,%a" pp_me me pp_mt mt
  | Val annot -> Pp.fp ppf "⟨val %a⟩" pp_annot annot
  | Extension_node ext -> Pp.fp ppf "%a" pp_extension ext
  | Abstract -> Pp.fp ppf "⟨abstract⟩"
  | Unpacked -> Pp.fp ppf "⟨unpacked⟩"
  | Open_me {opens = a :: q ; resolved; expr} ->
    Pp.fp ppf "%a.(%a)" Npath.pp a pp_me (Open_me{opens=q;resolved;expr})
  | Open_me {opens=[]; resolved; expr} ->
    Pp.fp ppf "⟨context:%a⟩ %a" D.pp resolved pp_me expr

and pp_mt ppf = function
  | Resolved fdefs -> Pp.fp ppf "✔%a" P.pp fdefs
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

module Build = struct
  let access path = Minor (Annot.access @@ Epath.prefix path)
  let open_ path = Open path
  let value v = Minor ( Annot.value v)
  let pack o = Minor (Annot.pack o)
end
