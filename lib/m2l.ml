
module M = Module

module Arg = M.Arg

module P = M.Partial

type 'a bind = {name:Name.t option; expr:'a}

type kind = Structure | Signature


type expression =
  | Open of module_expr (** [open A.B.C] ⇒ [Open [A;B;C]]  *)
  | Include of module_expr (** [struct include A end] *)
  | SigInclude of module_type
  (** [struct include A end] *)
  | Bind of module_expr bind
  (** [struct module A = struct … end] *)
  | Bind_sig of module_type bind   (** [struct module type A = sig … end] *)
  | Bind_rec of module_expr bind list
  (** [module rec A = … and B = … and …] *)
  | Minor of minor list
  (** value level expression.
      Invariant: for any pure interpreter [f], [ f (Minor m :: q ) ≡ f q ],
      i.e, this expression constructor is only meaningful for dependencies
      tracking.
 *)
  | Extension_node of extension
  (** [[%ext …]] *)


(** An annotation represents a short description of type or value level
    constructions, that contains only the information relevant for
    dependency tracking.
*)
and minor =
  | Access of access (** see {!access} below *)
  | Pack of module_expr Loc.ext (** (module struct ... end) *)
  | Extension_node of extension (** [%ext ... ] *)

  | Local_open of module_expr * minor list
  (** let open struct ... end in ... *)
  | Local_bind of  module_expr bind * minor list
and access = (Loc.t * Deps.Edge.t) Paths.E.map
  (** [M.N.L.x] ⇒ access \{M.N.L = Normal \}
      type t = A.t ⇒ access \{ A = ε \}
  *)

and module_expr =
  | Ident of Paths.Simple.t (** [A.B…] **)
  | Apply of {f: module_expr; x:module_expr} (** [F(X)] *)
  | Fun of module_expr fn (** [functor (X:S) -> M] *)
  | Constraint of module_expr * module_type (** [M:S] *)
  | Str of m2l (** [struct … end] *)
  | Val of minor list (** [val … ] *)
  | Extension_node of extension (** [[%ext …]] *)
  | Abstract
  (** empty module expression, used as a placeholder.
      In particular, it is useful for constraining first class module unpacking
      as [Constraint(Abstract, signature)]. *)
  | Unpacked (** [(module M)] *)
  | Open_me of { opens:Paths.Simple.t list; expr:module_expr}
  (** M.(…N.( module_expr)…)
      Note: This construction does not exist (yet?) in OCaml proper.
      It is used here to simplify the interaction between
      pattern open and first class module.*)
and module_type =
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
      deletions: Paths.S.set;
      minors: minor list
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
  | Val of minor list
and m2l = expression Loc.ext list
and 'a fn = { arg: module_type Arg.t option; body:'a }

type t = m2l

let annot_empty = []



(** Schematic serialization *)
module Sch = struct
  module S = Paths.S.Set
  open Schematic

  module Mu = struct
    let m2l, expr, module_expr, module_type, minor, extension, arg =
      Schematic_indices.seven
  end

  let access =
    let sch = Array [Paths.E.sch; Loc.Sch.t; Deps.Edge.sch] in
    let fwd x =
      Paths.E.Map.fold (fun k (x,y) l -> L.(Tuple.[k;x;y] :: l)) x L.[] in
    let rev a = let open Tuple in
      List.fold_left (fun m [k;x;y] ->Paths.E.Map.add k (x,y) m)
        Paths.E.Map.empty a in
    Custom {sch;fwd;rev}

  let loc sch = Custom { sch = [ sch; reopen Loc.Sch.t];
                          fwd = (fun x -> Tuple.[x.Loc.data;x.loc]);
                          rev = Tuple.(fun [data;loc] -> {data;loc});
                       }
  let module_expr_loc = loc Mu.module_expr
  let minor_sch =
    Sum [
      "Access", reopen access;
      "Pack", module_expr_loc;
      "Extension_node", Mu.extension;
      "Open", [Mu.module_expr; Array Mu.minor];
      "Bind", [option String; Mu.module_expr; Array Mu.minor];
    ]

  let annot = Array Mu.minor
  let me_raw =
    Sum [ "Ident", reopen Paths.S.sch;
          "Apply", [Mu.module_expr; Mu.module_expr];
          "Fun",[Mu.arg; Mu.module_expr];
          "Constraint",[Mu.module_expr; Mu.module_type];
          "Str",Mu.m2l; "Val", annot;
          "Extension_node", Mu.extension;
          "Abstract",Void;
          "Unpacked", Void;
          "Open_me",[Array (reopen Paths.S.sch); Mu.module_expr]
        ]

  let mt_sch =
    Sum [ "Alias", reopen Paths.S.sch;
          "Ident", reopen Paths.E.sch;
          "Sig", Mu.m2l;
          "Fun",[ Mu.arg; Mu.module_type ];
          "With",[ Mu.module_type; Array (Array String); annot ];
          "Of", Mu.module_expr;
          "Extension_node", Mu.extension;
          "Abstract", Void
        ]

  let expr  =
    let sch: ('a sum,_) s =
      Sum [ "Open", Mu.module_expr; "Include_me", Mu.module_expr;
            "SigInclude", Mu.module_type; "Bind", [option String; Mu.module_expr];
            "Bind_sig", [option String; Mu.module_type];
            "Bind_rec", Array [option String; Mu.module_expr];
            "Minor", annot; "Extension_node", Mu.extension ] in
    let fwd: _ -> 'a sum = let open Tuple in function
        | Open p -> C (Z p)
        | Include me -> C (S(Z me))
        | SigInclude mt -> C(S(S(Z mt)))
        | Bind {expr;name} -> C(S(S(S(Z [name;expr]))))
        | Bind_sig {expr;name} -> C(S(S(S(S(Z [name;expr])))))
        | Bind_rec rb ->
          C(S(S(S(S(S(Z (List.map (fun {expr;name} -> [name;expr]) rb)))))))
        | Minor a -> C(S(S(S(S(S(S(Z a)))))))
        | Extension_node ext -> C(S(S(S(S(S(S(S(Z ext))))))))
    in
    let rev:  'a sum -> _ = let open Tuple in function
        | C Z x -> Open x
        | C S Z x -> Include x
        | C S S Z x -> SigInclude x
        | C S S S Z [name;expr] -> Bind {name;expr}
        | C S S S S Z [name;expr] -> Bind_sig {name;expr}
        | C S S S S S Z x -> Bind_rec (List.map (fun [name;expr] -> {name;expr}) x)
        | C S S S S S S Z x -> Minor x
        | C S S S S S S S Z x -> Extension_node x
        | _ -> .
    in
    Custom{fwd; rev;sch}


  let rec module_expr =
    Custom{ sch = me_raw ; fwd = me_fwd; rev = me_rev }

  and me_fwd = let open Tuple in
    function
    | Ident x -> C(Z x)
    | Apply {f;x} -> C(S (Z [f;x]))
    | Fun {arg;body} -> C(S ( S (Z [arg;body])))
    | Constraint (x,y) -> C(S(S(S(Z[x;y]))))
    | Str x -> C(S(S(S(S(Z x)))))
    | Val x -> C(S(S(S(S(S(Z x))))))
    | Extension_node x -> C(S(S(S(S(S(S(Z x)))))))
    | Abstract -> C(S(S(S(S(S(S(S E)))))))
    | Unpacked -> C(S(S(S(S(S(S(S(S E))))))))
    | Open_me r -> C(S(S(S(S(S(S(S(S(S(Z [r.opens;r.expr]))))))))))
  and me_rev = let open Tuple in
    function
    | C Z x -> Ident x
    | C S Z [f;x] -> Apply{f;x}
    | C S S Z [arg;body] -> Fun{arg;body}
    | C S S S Z [x;y] -> Constraint(x,y)
    | C S S S S Z x -> Str x
    | C S S S S S Z x -> Val x
    | C S S S S S S Z x -> Extension_node x
    | C S S S S S S S E -> Abstract
    | C S S S S S S S S E -> Unpacked
    | C S S S S S S S S S Z [opens;expr] ->
      Open_me {opens;expr}
    | _ -> .


  let rec module_type =
    Custom { sch = mt_sch; fwd = mt_fwd; rev = mt_rev }
  and mt_fwd = let open Tuple in function
      | Alias x -> C (Z x)
      | Ident x -> C (S (Z x))
      | Sig x -> C(S (S (Z x)))
      | Fun x -> C(S (S (S (Z [x.arg; x.body]))))
      | With x -> C(S (S (S (S (Z [x.body; S.elements x.deletions; x.minors])))))
      | Of x ->  C(S (S (S (S (S (Z x))))))
      | Extension_node x ->  C(S (S (S (S (S (S (Z x)))))))
      | Abstract ->  C(S (S (S (S (S (S (S E)))))))
  and mt_rev = let open Tuple in function
      | C Z x -> Alias x
      | C S Z x -> Ident x
      | C S S Z x -> Sig x
      | C S S S Z [arg;body] -> Fun {arg;body}
      | C S S S S Z [body;deletions;minors] ->
        With {body;deletions = S.of_list deletions; minors}
      | C S S S S S Z x -> Of x
      | C S S S S S S Z x -> Extension_node x
      | C S S S S S S S E -> Abstract
      | _ -> .

  let expr_loc = loc Mu.expr
  let m2l = Array expr_loc

  let rec minor = Custom { sch = minor_sch; fwd; rev }
  and fwd = let open Tuple in function
      | Access x -> C (Z x)
      | Pack m -> C (S (Z m))
      | Extension_node e -> C (S (S (Z e)))
      | Local_open (x,y) -> C (S (S ( S (Z [x;y]))))
      | Local_bind(b,x) -> C (S (S (S ( S (Z [b.name; b.expr; x])))))
  and rev = let open Tuple in function
      | C Z x -> Access x
      | C S Z m -> Pack m
      | C S S Z e -> Extension_node e
      | C S S S Z [x;y] -> Local_open (x,y)
      | C S S S S Z [name;expr;z] -> Local_bind ({name;expr},z)
      | C E -> assert false
      | _ -> .

  let rec extension =
    Custom {sch = [ String; ext ];
            fwd = (fun {name;extension} -> Tuple.[name;extension] );
            rev = Tuple.(fun [name;extension] -> {name;extension} );
           }
  and ext =
    Custom {sch = Sum["Module", Mu.m2l;"Val", annot];
            fwd = ext_fwd; rev = ext_rev }
  and ext_fwd = function Module x -> C (Z x) | Val x-> C (S (Z x))
  and ext_rev =function C Z x -> Module x | C S Z x -> Val x | _ -> .

  let arg_raw =  Sum[ "None",Void; "Some", [option String; Mu.module_type] ]

  let rec arg =
    Custom { sch = arg_raw;
             fwd = arg_fwd;
             rev = arg_rev;
           }
  and arg_fwd = function
    | None -> C E
    | Some (a:module_type Module.Arg.t) -> C(S(Z Tuple.[a.name;a.signature]))
  and arg_rev = let open Tuple in
    function C E -> None | C S Z [n;s] -> Some {name=n;signature=s} | _ -> .

  let defs: _ rec_defs =
    [
      "m2l", m2l;
      "expr", expr;
      "module_expr", module_expr;
      "module_type", module_type;
      "minor", minor;
      "extension", extension;
      "arg",arg;
    ]

  let id = L.["m2l"]
  let m2l =
    Rec { id; defs; proj = Zn }
  let expr = Rec { id; defs; proj = Sn Zn }
  let module_expr = Rec { id; defs; proj = Sn(Sn Zn) }
  let module_type = Rec { id; defs; proj = Sn(Sn(Sn Zn)) }
  let minor = Rec { id; defs;  proj = Sn(Sn(Sn(Sn Zn))) }
  let minors = Array minor

end let sch = Sch.m2l


module Annot = struct
  open Loc
  type t = minor list Loc.ext
  let empty = { data = annot_empty; loc = Nowhere }
  let is_empty x  = x.data = annot_empty

  module Access = struct
    type t = access
    let merge = Paths.E.Map.merge (fun _k x y -> match x, y with
        | Some (l,x), Some (l',y) ->
          if x = Deps.Edge.Normal && y = Deps.Edge.Epsilon then
            Some(l',y)
          else
            Some(l, Deps.Edge.max x y)
        | None, (Some _ as x) | (Some _ as x), None -> x
        | None, None -> None
      )

    let empty = Paths.E.Map.empty
  end

  let merge x y =
    let a1 = x.data and a2 = y.data in
    {Loc.data = a1 @ a2; loc = Loc.merge x.loc y.loc}


  let (++) = merge

  let union l =
    List.fold_left (++) empty l
  let union_map f l =
    List.fold_left (fun res x -> res ++ f x ) empty l

  let access {loc;data} =
    Loc.create loc
    [Access (Paths.E.Map.singleton data (loc, Deps.Edge.Normal))]

  let abbrev {loc;data} =
    Loc.create loc
      [Access (Paths.E.Map.singleton data (loc, Deps.Edge.Epsilon))]


  let pack x = { x with data = [Pack x] }
  let ext {loc;data} = Loc.create loc [(Extension_node data: minor)]

  let local_bind mb {loc;data} = { loc; data = [Local_bind(mb,data)] }
  let local_open me {loc;data} = { loc; data = [Local_open (me,data)] }
 
  let opt f x = Option.( x >>| f >< empty )

  let rec epsilon_promote_raw = function
    | Access x ->
      let m = Paths.E.Map.map (fun (l,_) -> l, Deps.Edge.Epsilon) x in
      Access m
    | Pack _ | Extension_node _ as p -> p
    | Local_open (me,x) -> Local_open(me, List.map epsilon_promote_raw x)
    | Local_bind (b,x) -> Local_bind (b, List.map epsilon_promote_raw x)

  let epsilon_promote = Loc.fmap @@ List.map epsilon_promote_raw

end

(** Helper function *)
module Build = struct
  let ghost data  = {Loc.data; loc= Nowhere }

  let minor x = Minor x
  let access path =
    Loc.fmap (fun x -> minor x) @@ Annot.access path

  let open_ path = Loc.fmap (fun x -> Open x) path
  let open_path x = open_ (Loc.fmap (fun x -> Ident x) x)

  let open_me ml expr = match expr with
    | Open_me o -> Open_me { o with opens = ml @ o.opens }
    | expr ->  Open_me {  opens = ml; expr }

  let fn_sig arg : module_type = Fun arg
  let fn arg : module_expr  = Fun arg

end

let rec pp_expression ppf = function
  | Minor annot -> pp_annot ppf annot
  | Open me -> Pp.fp ppf "@[open [%a]@]" pp_me me
  | Include me -> Pp.fp ppf "@[include [%a]@]" pp_me me
  | SigInclude mt -> Pp.fp ppf "@[include type [%a]@]" pp_mt mt

  | Bind bind -> pp_bind ppf bind
  | Bind_sig bind -> pp_bind_sig ppf bind
  | Extension_node e -> pp_extension ppf e
  | Bind_rec bs ->
    Pp.fp ppf "rec@[[ %a ]@]"
      (Pp.(list ~sep:(s "@, and @,")) @@ pp_bind ) bs

and pp_packed ppf packed =
  let sep = Pp.s ";@ " in
  let post = Pp.s "@]" in
  Pp.(opt_list ~sep ~pre:(s "@,@[<2>packed: ") ~post pp_opaque) ppf packed
and pp_values ppf values =
  Pp.(opt_list ~sep:(s ";@ ") ~pre:(s "@[<2>values: ") ~post:(s "@]")
        pp_simple) ppf values
and pp_minor ppf = function
  | Access a -> pp_access ppf a
  | Pack x -> Pp.fp ppf "@[(module %a)@]" pp_me x.Loc.data
  | Extension_node e -> pp_extension ppf e
  | Local_open (me,x) ->
    Pp.fp ppf "@[<2>open %a in@ %a@]" pp_me me pp_annot x
  | Local_bind (b,x) ->
    let name = Option.( b.name >< "_" ) in
    Pp.fp ppf "@[<2>%s=%a in@ %a@]" name pp_me b.expr pp_annot x
and pp_annot ppf l =
  Pp.(list ~pre:(s "@[<v>") ~post:(s "@]") ~sep:(s "@ ") pp_minor) ppf l
and pp_access ppf s =  if Paths.E.Map.cardinal s = 0 then () else
    Pp.fp ppf "@[<2>access: {%a}@]@," (Pp.list pp_access_elt) (Paths.E.Map.bindings s)
and pp_access_elt ppf (name, (loc,edge)) =
  Pp.fp ppf "%s%a(%a)" (if edge = Deps.Edge.Normal then "" else "ε∙")
    Paths.E.pp name
    Loc.pp loc
and pp_opaque ppf me = Pp.fp ppf "⟨%a(%a)⟩" pp_me me.Loc.data Loc.pp me.loc
and pp_bind ppf {name;expr} =
  match expr with
  | Constraint(Abstract, Alias np) ->
    Pp.fp ppf "@[module %a ≡ %a@]" Name.pp_opt name Paths.Simple.pp np
  | Constraint(Abstract, mt) ->
    Pp.fp ppf "@[<2>module %a:%a@]" Name.pp_opt name pp_mt mt
  | Constraint(Unpacked, mt) ->
    Pp.fp ppf "@[<2>(module %a:%a)@]" Name.pp_opt name pp_mt mt
  | Unpacked ->
    Pp.fp ppf "(module %a)" Name.pp_opt name
  | Open_me {opens = a :: q ; expr} ->
    Pp.fp ppf "%a.(%a)" Paths.Simple.pp a pp_bind
      {name; expr = Open_me{opens=q;expr} }
  | Open_me {opens=[]; expr} ->
    Pp.fp ppf "%a"
       pp_bind {name;expr}
  | _ ->
    Pp.fp ppf "@[<2>module %a =@ %a@]" Name.pp_opt name pp_me expr
and pp_bind_sig ppf {name;expr} =
  match expr with
  | Alias id ->
    Pp.fp ppf "@[module type %a ≡ %a@]" Name.pp_opt name Paths.Simple.pp id
  | Abstract ->
    Pp.fp ppf "@[module type %a@]" Name.pp_opt name
  | _ ->
    Pp.fp ppf "@[<2>module type %a =@ %a @]" Name.pp_opt name pp_mt expr
and pp_me ppf = function
  | Ident np -> Paths.Simple.pp ppf np
  | Str m2l -> Pp.fp ppf "@,struct@, %a end" pp m2l
  | Apply {f;x} -> Pp.fp ppf "%a(@,%a@,)" pp_me f pp_me x
  | Fun { arg; body } -> Pp.fp ppf "%a@,→%a" (Arg.pp pp_mt) arg pp_me body
  | Constraint (me,mt) -> Pp.fp ppf "%a: @,%a" pp_me me pp_mt mt
  | Val annot -> Pp.fp ppf "⟨val %a⟩" pp_annot annot
  | Extension_node ext -> Pp.fp ppf "%a" pp_extension ext
  | Abstract -> Pp.fp ppf "⟨abstract⟩"
  | Unpacked -> Pp.fp ppf "⟨unpacked⟩"
  | Open_me {opens = a :: q ; expr} ->
    Pp.fp ppf "%a.(%a)" Paths.Simple.pp a pp_me (Open_me{opens=q;expr})
  | Open_me {opens=[]; expr} ->
    Pp.fp ppf "%a" pp_me expr

and pp_mt ppf = function
  | Alias np -> Pp.fp ppf "(≡)%a" Paths.Simple.pp np
  | Ident np -> Paths.Expr.pp ppf np
  | Sig m2l -> Pp.fp ppf "@,sig@, %a end" pp m2l
  | Fun { arg; body } ->  Pp.fp ppf "%a@,→%a" (Arg.pp pp_mt) arg pp_mt body
  | With {body; deletions;minors} ->
    Pp.fp ppf "%a@,/%a (%a)" pp_mt body Paths.S.Set.pp deletions pp_annot minors
  | Of me -> Pp.fp ppf "module type of@, %a" pp_me me
  | Extension_node ext -> Pp.fp ppf "%a" pp_extension ext
  | Abstract -> Pp.fp ppf "⟨abstract⟩"
and pp_extension ppf x = Pp.fp ppf "[%%%s @[%a@]]" x.name pp_extension_core
    x.extension
and pp_extension_core ppf = function
  | Module m -> pp ppf m
  | Val m -> pp_annot ppf m
and pp_simple ppf =
  Pp.list ~sep:(Pp.s " @,") pp_expression_with_loc ppf
and pp ppf = Pp.fp ppf "@[<2>[@,%a@,]@]" pp_simple
and pp_expression_with_loc ppf e = Pp.fp ppf "%a(%a)"
    pp_expression e.data Loc.pp e.loc


module Sig_only = struct

  let (|||) (b,x) (b',y) = b || b', (x,y)
  let (@::) a (b,l) = (b, a :: l)

  let map f (b, x) = (b, f x)


  let sig_arg inner = function
    | None as a -> false, a
    | Some r ->
      map (fun s -> Some { r with Arg.signature = s }) (inner r.signature )

  let rec rev keep_opens = function
    | { Loc.data = Minor _; _ } :: l -> rev keep_opens l
    | { data= Open _; _ }  as o :: q ->
      if keep_opens then  o @:: rev keep_opens q
      else rev keep_opens q
    | { data = Extension_node _; _ } as a :: q  -> a @:: rev true q
    | { data = (SigInclude _| Include _); _ }  as a :: q -> a @:: rev true q
    | { data = Bind {name;expr}; loc }  :: q ->
      let k, expr = mex expr in
      { Loc.data = Bind{name;expr}; loc } @:: rev (k||keep_opens) q
    | { data = (Bind_sig  _ | Bind_rec _); _ }  as a :: q
      -> a @:: rev true q
    | [] -> false, []
  and main l = map List.rev (rev false @@ List.rev l)
  and mex = function
    | ( Val _ | Abstract | Unpacked )  as a-> false, a
    | (Ident _ | Apply _ | Extension_node _ ) as a -> true, a
    | Fun {arg;body} ->
      let b, arg = sig_arg mty arg in
      let b', body = mex body in
      b || b', Fun {arg;body}
    | Constraint (me,mt) ->
      map (fun (x,y) -> Constraint (x,y)) (mex me ||| mty mt)
    | Str s ->
      map (fun x -> Str x) (main s)
    | Open_me ({ expr; _ } as r) ->
      let b, expr = mex expr in
      b , Open_me { r with expr}
  and mty = function
    | (Abstract as a ) -> false, a
    | (Alias _ | Ident _ | Extension_node _ as a ) -> true, a
    | Sig code -> map (fun x -> Sig x) (main code)
    | Fun {arg;body} ->
      let k, arg = sig_arg mty arg in
      let k', body = mty body in
      k || k', Fun {arg;body}
    | With ({ body; _ } as r) ->
      map (fun x -> With { r with body=x}) (mty body)
    | Of e -> map (fun x -> Of x) (mex e)

  let filter m2l = snd @@ main m2l

end
