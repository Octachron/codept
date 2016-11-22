
module M = Module

module Arg = M.Arg
module D = Definition
module Def = D.Def

module P = M.Partial

type 'a bind = {name:Name.t; expr:'a}

type expression =
  | Defs of Definition.t (** Resolved module actions M = … / include … / open … *)
  | Open of Paths.Simple.t (** [open A.B.C] ⇒ [Open [A;B;C]]  *)
  | Include of module_expr (** [struct include A end] *)
  | SigInclude of module_type
  (** [struct include A end] *)
  | Bind of module_expr bind
  (** [struct module A = struct … end] *)
  | Bind_sig of module_type bind   (** [struct module type A = sig … end] *)
  | Bind_rec of module_expr bind list
  (** [module rec A = … and B = … and …] *)
  | Minor of annotation
  (** value level expression.
      Invariant: for any pure interpreter [f], [ f (Minor m :: q ) ≡ f q ],
      i.e, this expression constructor is only meaningful for dependencies
      tracking.
 *)
  | Extension_node of extension
  (** [[%ext …]] *)
and annotation =
  { access: Name.set (** [M.N.L.x] ⇒ access \{M\} *)
  ; values: m2l list (** − [let open A in …] ⇒ [[ Open A; …  ]]
                         − [let module M = … ] ⇒ [[ Include A; … ]]
                     *)
  ; packed: module_expr list (** [(module M)] *)
  }
and module_expr =
  | Resolved of P.t
  (** Already resolved module expression, generally
      used in subexpression when waiting for other parts
      of module expression to be resolved
  *)
  | Ident of Paths.Simple.t (** [A.B…] **)
  | Apply of {f: module_expr; x:module_expr} (** [F(X)] *)
  | Fun of module_expr fn (** [functor (X:S) -> M] *)
  | Constraint of module_expr * module_type (** [M:S] *)
  | Str of m2l (** [struct … end] *)
  | Val of annotation (** [val … ] *)
  | Extension_node of extension (** [[%ext …]] *)
  | Abstract
  (** empty module expression, used as a placeholder.
      In particular, it is useful for constraining first class module unpacking
      as [Constraint(Abstract, signature)]. *)
  | Unpacked (** [(module M)] *)
  | Open_me of { resolved: Definition.t; opens:Paths.Simple.t list; expr:module_expr}
  (** M.(…N.( module_expr)…)
      Note: This construction does not exist (yet?) in OCaml proper.
      It is used here to simplify the interaction between
      pattern open and first class module.*)
and module_type =
  | Resolved of P.t (** same as in the module type *)
  | Alias of Paths.Simple.t (** [module m = A…]  *)
  | Ident of Paths.Expr.t
  (** module M : F(X).s
      Note: Paths.Expr is used due to [F.(X).s] expressions
      that do not have an equivalent on the module level
 *)
  | Sig of m2l (** [sig … end] *)
  | Fun of module_type fn (** [functor (X:S) → M] *)
  | With of {
      body: module_type;
      deletions: Name.set
      (* ; equalities: (Npath.t * Epath.t) list *)
    }
  (** [S with module N := …]
      we are only tracking module level modification
  *)
  | Of of module_expr (** [module type of …] *)
  | Extension_node of extension (** [%%… ] *)
  | Abstract (** placeholder *)
and extension = {name:string; extension:extension_core}
and extension_core =
  | Module of m2l
  | Val of annotation
and m2l = expression list
and 'a fn = { arg: module_type Arg.t option; body:'a }

type t = m2l

(** The Block module computes the first dependencies needed to be resolved
    before any interpreter can make progress evaluating a given code block *)
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
    | Ident e -> Some (Paths.Expr.prefix e)
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

(** Helper function *)
module Build = struct
  let access path = Minor (Annot.access @@ Paths.Expr.prefix path)
  let open_ path = Open path
  let value v = Minor ( Annot.value v)
  let pack o = Minor (Annot.pack o)

  let open_me ml expr = match expr with
    | Open_me o -> Open_me { o with opens = ml @ o.opens }
    | expr ->  Open_me { resolved = D.empty; opens = ml; expr }

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

end


let rec pp_expression ppf = function
  | Defs defs -> Pp.fp ppf "define %a" D.pp defs

  | Minor annot -> pp_annot ppf annot
  | Open npath -> Pp.fp ppf "@[<hv>open %a@]" Paths.Simple.pp npath
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
    Pp.fp ppf "@[module %s ≡ %a" name Paths.Simple.pp np
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
    Pp.fp ppf "@[module type %s ≡ %a @]" name Paths.Simple.pp id
  | Abstract ->
    Pp.fp ppf "@[module type %s@]" name
  | _ ->
    Pp.fp ppf "@[module type %s = @,@[<hv>%a@] @]" name pp_mt expr
and pp_me ppf = function
  | Resolved fdefs -> Pp.fp ppf "✔%a" P.pp fdefs
  | Ident np -> Paths.Simple.pp ppf np
  | Str m2l -> Pp.fp ppf "@,struct@, %a end" pp m2l
  | Apply {f;x} -> Pp.fp ppf "%a(@,%a@,)" pp_me f pp_me x
  | Fun { arg; body } -> Pp.fp ppf "%a@,→%a" (Arg.pp pp_mt) arg pp_me body
  | Constraint (me,mt) -> Pp.fp ppf "%a: @,%a" pp_me me pp_mt mt
  | Val annot -> Pp.fp ppf "⟨val %a⟩" pp_annot annot
  | Extension_node ext -> Pp.fp ppf "%a" pp_extension ext
  | Abstract -> Pp.fp ppf "⟨abstract⟩"
  | Unpacked -> Pp.fp ppf "⟨unpacked⟩"
  | Open_me {opens = a :: q ; resolved; expr} ->
    Pp.fp ppf "%a.(%a)" Paths.Simple.pp a pp_me (Open_me{opens=q;resolved;expr})
  | Open_me {opens=[]; resolved; expr} ->
    Pp.fp ppf "⟨context:%a⟩ %a" D.pp resolved pp_me expr

and pp_mt ppf = function
  | Resolved fdefs -> Pp.fp ppf "✔%a" P.pp fdefs
  | Alias np -> Pp.fp ppf "(≡)%a" Paths.Simple.pp np
  | Ident np -> Paths.Expr.pp ppf np
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


(** {Normalize} computes the normal form of a given m2l code fragment *)
module Normalize = struct

  let halt l = false, l
  let continue l = true, l


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
